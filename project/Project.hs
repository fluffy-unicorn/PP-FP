module Project where
import Data.Maybe
import Data.List
import DataModel
import Debug.Trace

{--
    Level 1
--}
-- Return the right-hand side of an atom, or itself when it is not defined
substitute :: Program -> Atom -> [Atom]
substitute prog a = case lookup a prog of
                    Just as -> as
                    Nothing -> [a]

-- Evaluate function for level 1
evalProp :: Program -> Query -> Bool
evalProp prog q = evalProp' prog q == []

-- Recursive function for evalProp
evalProp' :: Program -> Query -> [Atom]
evalProp' _ [] = []
evalProp' prog (q:qs) | sqs == [q] = (q:qs)
                      | otherwise = [result | result <- evalProp' prog (sqs ++ qs) ] 
                      where
                        sqs = substitute prog q

{--
    Level 2
--}
-- Indicates whether two atoms can be matched with each other
match :: Atom -> Atom -> Bool
match (P p _) (P q (V _)) = p == q
match (P p (V _)) (P q _) = p == q
match (PMult p x) (PMult q y) = p == q && length x == length y && matchArray x y   
match x y = x == y

-- Indicates whether two arrays of terms can be matched with each other
matchArray :: [Term] -> [Term] -> Bool
matchArray [] [] = True
matchArray [] _ = False
matchArray _ [] = False
matchArray (a:as) (b:bs) = case (a, b) of
                           (V _, _) -> matchArray as bs
                           (_, V _) -> matchArray as bs
                           (C a, C b)-> a == b && matchArray as bs

-- Gives all the clauses that match with the given atom
matches :: Program -> Atom -> Maybe [Clause]
matches prog a | result == [] = Nothing
                     | otherwise = Just result
                       where result = [ rule | rule <- prog, match (fst rule) a ]

-- Rename an atom
rename :: Atom -> Atom
rename (P p (C a)) = (P p (C a))
rename (P p (V x)) = (P p (V ('_':x)))
rename a = a

-- Get the substitution associated with the given atoms
unify :: Atom -> Atom -> Maybe Substitution
unify (P _ (C a)) (P _ (C b)) = Nothing
unify (P p (C a)) (P q (V v)) | p == q = Just (V v, C a)
                                      | otherwise = Nothing
unify (P p (V v)) (P q (C a)) | p == q = Just (C a, V v)
                                      | otherwise = Nothing
unify (P p (V v1)) (P q (V v2)) | p == q = Just (V v2, V v1)
                                      | otherwise = Nothing
unify _ _ = Nothing

-- 'Beautify' the solution given by evalOne
beautify :: Solution -> (Bool, [String])
beautify (q, s) | q /= [] = (False, [])
                | otherwise = (True, nub $ map (showSubstitution.fromJust) $ filter beautifyFilter s)

-- Filter used by the beautify function
beautifyFilter :: Maybe Substitution -> Bool
beautifyFilter Nothing = False
beautifyFilter (Just (V _, C _)) = False
beautifyFilter _ = True

-- String representation of a substitution
showSubstitution :: Substitution -> String
showSubstitution (C a, V v) = v ++ " = " ++ show a
showSubstitution (V v, V w) = v ++ " = " ++ w
showSubstitution (V v, C a) = show a ++ " = " ++ v

-- Evaluate function for level 2
evalOne :: Program -> Query -> (Bool, [String])
evalOne p q = beautify $ evalOne' p (q, [Nothing])

-- Recursive function for evalOne and evalMult
evalOne' :: Program -> Solution -> Solution
evalOne' p ([], sub) = ([], sub)
evalOne' p ((q:query),sub) | result == (q:query) = ((q:query), sub)
                           | otherwise = evalOne' p (result, sub ++ (subAs `intersectOrUnion` subRF))
                            where result = atomResults ++ restResult
                                  evalAtomResults = evalAtom p q
                                  atomResults = foldl1 (++) $ map fst evalAtomResults
                                  subAs = filter (/=Nothing) $ foldl1 (++) $ map snd evalAtomResults
                                  (restResult,subR) = evalOne' p (query, sub)
                                  subRF = filter (/=Nothing) subR

-- Evaluate a single atom
evalAtom :: Program -> Atom -> [Solution]
evalAtom p (P pr x)     | matchResult == Nothing = [([P pr x], [Nothing])]
                        | otherwise = (map (unifyAndEval p (P pr x)) $ fromJust matchResult) 
                         where matchResult = matches p (P pr x) 
evalAtom p (PMult pr x)  | matchResult == Nothing = [([PMult pr x], [Nothing])]
                         | otherwise = (map (unifyAndEvalMult p (PMult pr x)) $ fromJust matchResult) 
                           where matchResult = matches p (PMult pr x) 
evalAtom p a = [(evalProp' p [a], [Nothing])]

-- Unify and evaluate an atom with a given clause
unifyAndEval :: Program -> Atom -> Clause -> Solution
unifyAndEval p atom c = case unification of
                          Nothing -> evalOne' p (snd c, [Nothing])
                          Just (V _, _) -> evalOne' p (substitution, [Nothing])
                          _             -> evalOne' p (substitution, [unification])
                        where unification = unify atom $ fst c
                              substitution= [ a ⇐ fromJust unification | a <- snd c ]

{--
    Level 3
--}
-- Unify a multiple parameter atom
unifyMult :: Atom -> Atom -> [Maybe Substitution]
unifyMult (PMult _ []) _ = []
unifyMult _ (PMult _ []) = []
unifyMult (PMult p (x:xs)) (PMult q (y:ys)) | p /= q = [Nothing]
                                            | otherwise = correct $ [unify (P p x) (P q y)] ++ unifyMult (PMult p xs) (PMult q ys)

-- Correct the generated list for instances as p(X,X) = p(a,Y)
correct :: [Maybe Substitution] -> [Maybe Substitution]
correct x = vc ++ vv ++ correctedList
            where (vc, vv) = partition isVarToConst $ filter (/=Nothing) x
                  result = [ correctSub v vc | v <- vv ]
                  correctedList | result == [] = []
                                | otherwise = foldl1 (++) result

-- Return a list of new substitutions with a given Var-to-Var-substition and a list of Var-to-Constant-substition
correctSub :: Maybe Substitution -> [Maybe Substitution] -> [Maybe Substitution]
correctSub _ [] = []
correctSub (Just (V v, V w)) (x:xs) | V v == fst fjx = [Just (snd fjx, V w)] ++ correctSub (Just (V v, V w)) xs
                                    | V w == fst fjx = [Just (V v, snd fjx)] ++ correctSub (Just (V v, V w)) xs
                                    | otherwise = []
                                    where fjx = fromJust x

-- Evaluate function for level 3
evalMult :: Program -> Query -> (Bool, [String])
evalMult p q = beautify $ evalOne' p (q, [Nothing])

-- Unify and evaluate an atom with a given clause (for level 3)
unifyAndEvalMult :: Program -> Atom -> Clause -> Solution
unifyAndEvalMult p atom c = case unification of
                          [Nothing] -> evalOne' p (snd c, [])
                          _         -> evalOne' p (substitution, unification)
                        where unification = unifyMult atom $ fst c
                              substitution = [ foldl (⇐) a $ map fromJust unification | a <- snd c ]
{--
    Dictionary implementation
--}
-- Convert a list to an dictionary
listToDict :: [Maybe Substitution] -> Dictionary -> Dictionary
listToDict [] dict = dict
listToDict (a:as) dict | a == Nothing = listToDict as dict
                       | otherwise = listToDict' (a:as) dict

-- Recursive function for listToDict
listToDict' :: [Maybe Substitution] -> Dictionary -> Dictionary
listToDict' (a:as) dict = case lookup (snd fja) dict of
                           Nothing -> listToDict as (dict ++ [(snd fja, [fst fja])])
                           Just lst -> listToDict as (addIntoDict dict (snd fja) (lst ++ [fst fja]))
                          where fja = fromJust a

-- Add a value into the dictionary, replacing an existing value
addIntoDict :: Dictionary -> Term -> [Term] -> Dictionary
addIntoDict [] _ _ = []                    
addIntoDict (d:dict) key value | fst d == key = ((key, value) : dict) 
                               | otherwise = (d : addIntoDict dict key value)

-- Get the value associated with the given key in the dictionary
getDictValue :: Dictionary -> Term -> [Term]
getDictValue [] key = []
getDictValue (d:dict) key | fst d == key = snd d
                          | otherwise = getDictValue dict key 

-- Calculate the intersection of two dictionaries
intersectDict :: Dictionary -> Dictionary -> Dictionary -> Dictionary
intersectDict [] []     cs = cs
intersectDict [] (b:bs) cs | any (==key) $ map fst cs = intersectDict [] bs cs
                           | otherwise = intersectDict [] bs (cs ++ [b])
                           where key = fst b
intersectDict (a:as) bs cs | any (==key) $ map fst bs = intersectDict as bs (cs ++ [(key, snd a `intersect` (getDictValue bs key))])
                           | otherwise = intersectDict as bs (cs ++ [a])
                           where key = fst a

-- Convert a dictionary to a list
dictToList :: Dictionary -> [Maybe Substitution]
dictToList [] = []
dictToList (d:dict) = [ Just (b, fst d) | b <- snd d ] ++ dictToList dict

-- Function that calculates the intersection of substitions
-- This intersection is on a variable basis, so intersectOrUnion {V=a, V=b} {V=b} yields {V=b}
-- whereas intersectOrUnion {V=a} {W=b} yields {V=a, W=b} 
intersectOrUnion :: [Maybe Substitution] -> [Maybe Substitution] -> [Maybe Substitution]
intersectOrUnion as bs = dictToList $ intersectDict aDict bDict []
                       where
                         aDict = listToDict (filter isVarToConst as) []
                         bDict = listToDict (filter isVarToConst bs) []

-- Filter for intersectOrUnion
isVarToConst :: Maybe Substitution -> Bool
isVarToConst (Just (V _, V _)) = False
isVarToConst Nothing = False
isVarToConst _ = True

{--
    Tests
--}
testProp = evalProp program
testOne  = evalOne  program
testMult = evalMult program
printTest x = putStr $ unlines $ map show x

-- Level 1
testProp1 = testProp [C0] -- True
testProp2 = testProp [C1] -- False

-- Level 2
testOne1  = testOne [P "q" (C A)]     -- True
testOne2  = testOne [P "q" (C C_)]    -- False
testOne3  = testOne [P "q" (V "Z")]   -- True (Z = a, Z = b)
testOne4  = testOne [P "r" (C A)]     -- True
testOne5  = testOne [P "r" (C C_)]    -- False
testOne6  = testOne [P "r" (V "Z")]   -- True (Z = a, Z = b)
testOne7  = testOne [P "r" (C D)]     -- False
testOneProp1 = testOne [C0]           -- True
testOneProp2 = testOne [C1]           -- False

-- Level 3
testMult1 = testMult [PMult "s" [V "X", V "Y"]] -- True (X = a, Y = b)
testMult2 = testMult [PMult "t" [V "X", V "Y"]] -- True (X = a, X = b, X = c, Y = b)
testMult3 = testMult [PMult "v" [C A, V "Y"]]   -- True (Y = a)
testMultOne1 = testMult [P "r" (V "Z")]         -- True (Z = a, Z = b)
testMultProp1= testMult [C0]                    -- True

testProps = printTest [testProp1, testProp2]
testOnes  = printTest [testOne1, testOne2, testOne3, testOne4, testOne5, testOne6, testOne7, testOneProp1, testOneProp2 ]
testMults = printTest [testMult1, testMult2, testMult3, testMultOne1, testMultProp1 ]














