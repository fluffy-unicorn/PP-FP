module Project where
import Data.Maybe
import Data.List
import DataModel

{--
    Exercise 1
--}
substitute :: Program -> Atom -> [Atom]
substitute prog a = case lookup a prog of
                    Just as -> as
                    Nothing -> [a]

evalProp :: Program -> Query -> Bool
evalProp prog q = evalProp' prog q == []

evalProp' :: Program -> Query -> [Atom]
evalProp' _ [] = []
evalProp' prog (q:qs) | sqs == [q] = (q:qs)
                      | otherwise = [result | result <- evalProp' prog (sqs ++ qs) ] 
                      where
                        sqs = substitute prog q

{--
    Exercise 2
--}
match :: Atom -> Atom -> Bool
match (Pr p x) (Pr q y) | p==q = case (x, y) of
                                (Var _, _) -> True
                                (_, Var _) -> True
                                (a,b)      -> x == y
                        | otherwise = False     
match x y = x == y

matches :: Atom -> Program -> [[Atom]]
matches (Pr p (Var x)) prog = [snd mtch | mtch <- prog, match (fst mtch) (Pr p (Var x))]
matches a prog = case lookup a prog of
                 Just as -> [as]
                 Nothing -> [[a]]

rename :: Term -> Term
rename (Const a) = Const a
rename (Var x) = Var ('_':x)

unify :: Atom -> Atom -> Maybe Substitution
unify (Pr _ (Const _)) (Pr _ (Const _)) = Nothing
unify (Pr p (Const a)) (Pr q (Var v)) | p == q = Just (Var v, Const a)
                                      | otherwise = Nothing
unify (Pr p (Var v)) (Pr q (Const a)) | p == q = Just (Var v, Const a)
                                      | otherwise = Nothing
unify (Pr p (Var v1)) (Pr q (Var v2)) | p == q = Just (Var v1, Var v2)
                                      | otherwise = Nothing
unify _ _ = Nothing

--evalOne :: Program -> Query -> Bool
--evalOne prog q = evalOne' prog q == []

--evalOne' :: Program -> Query -> [Atom]
evalOne' prog (q:query) | allSubQ 
                          where 
                            allSubQ = substituteAll query prog
--evalOne' _ [] = []
--evalOne' prog (query:qs) | unifiedQueries == [[query]] = (query:qs)
--                         where
--                            substitutions = unifyQuery query prog
--                            unifiedQueries = [ fst (query ⇐ sub) | sub <- substitutions]
--evalOne' prog (q:qs) = [unification | unification <- (unifyTerms q prog), let sq = q ⇐ unification, evalProp prog [sq] ]

-- Return all the possible substitutions with a given atom
unifyTerm :: Atom -> Program -> [Substitution]
unifyTerm a prog = [ fromJust(unifiedQ) | q <- prog, let unifiedQ = unify (fst q) a,  unifiedQ /= Nothing ]

-- Return all the possible substitutions with a given query
unifyQuery :: Query -> Program -> [Substitution]
unifyQuery as prog =  foldl union [] [ unifyTerm s prog | s <- as]

-- Return the substituted query
substituteQuery :: Query -> Substitution -> Query
substituteQuery query sub = [q ⇐ sub | q <- query]

-- Return all the possible substituted queries 
substituteAll :: Query -> Program -> [Query]
substituteAll query prog = [ substituteQuery query sub | sub <- unifyQuery query prog ]



