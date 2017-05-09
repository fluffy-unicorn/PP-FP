module Session4 where
import FPPrac.Trees
import Data.List
--1a
data BinTree a b = Leaf b | Node a (BinTree a b ) (BinTree a b)

--b
data Unit = Unit
instance Show Unit where
    show x = ""
type Tree1a = BinTree Int Int
type Tree1b = BinTree (Int, Int) (Int, Int) 
type Tree1c = BinTree Unit Int
type Tree4  = BinTree Int Unit

--c
pp :: (Show a, Show b) => BinTree a b -> RoseTree
pp (Leaf x) = RoseNode (show x) []
pp (Node x t1 t2) = RoseNode (show x) [pp t1, pp t2]

--2a
isDigit :: Char -> Bool
isDigit x = x `elem` ['0'..'9']

isAlpha :: Char -> Bool
isAlpha x = x `elem` ['A'..'z'] \\ ['['..'`']

parse :: String -> ((BinTree Char Int), String)
parse "" = (Leaf 0, "")
parse (x:xs) | isDigit x = (Leaf (read [x]::Int), xs)
             | x == '('  = ((Node op (t0) (t1)), tail r2) 
             where
               (t0, r0) = parse xs
               (op, r1) = (head r0, tail r0)
               (t1, r2) = parse r1

parseTest = showRoseTree $ pp $ fst $ parse "((6*((2^8)-(3/2)))+3)"

--b

parse' :: String -> ((BinTree Char (Either Int Char)), String)
parse' "" = (Leaf (Left 0), "")
parse' (x:xs) | isDigit x = (Leaf (Left (read[x]::Int)), xs)
              | isAlpha x = (Leaf (Right x), xs)
              | x == '('  = ((Node op (t0) (t1)), tail r2) 
              where
               (t0, r0) = parse' xs
               (op, r1) = (head r0, tail r0)
               (t1, r2) = parse' r1

parseTest' = showRoseTree $ pp $ fst $ parse' "((a*((2^c)-(3/d)))+3)"

data State = Init | Num1 | Num2 | Num3 | Idf1 | OpsPlus | OpsDiv | OpsEq | OpsSub | OpsNEq | OpsAcc | Bracket1 | WS1 | Error deriving Show

fsaNum :: State -> Char -> State
fsaNum Init x | isDigit x || x == '~' = Num1
fsaNum Num1 x | isDigit x = Num1
              | x == '.'  = Num2
fsaNum Num2 x | isDigit x = Num3
fsaNum _    _ = Error

fsaIdf :: State -> Char -> State 
fsaIdf Init x | isAlpha x = Idf1
fsaIdf Idf1 x | isDigit x || isAlpha x = Idf1
fsaIdf _    _ = Error

fsaOps :: State -> Char -> State
fsaOps Init    x | x `elem` "*^%" = OpsAcc
                 | x `elem` "<>"  = OpsNEq
                 | x == '+' = OpsPlus
                 | x == '-' = OpsSub
                 | x == '/' = OpsDiv
                 | x == '=' = OpsEq
fsaOps OpsPlus x | x == '+' = OpsAcc
fsaOps OpsSub  x | x == '-' = OpsAcc
fsaOps OpsDiv  x | x == '/' = OpsAcc
fsaOps OpsEq   x | x == '=' = OpsAcc
fsaOps OpsNEq  x | x == '=' = OpsAcc
fsaOps _       _ = Error

fsaBra :: State -> Char -> State
fsaBra Init x | x == '(' || x == ')' = Bracket1
fsaBra _    _ = Error

fsaWS :: State -> Char -> State
fsaWS Init x | x == ' ' = WS1
fsaWS WS1  x | x == ' ' = WS1
fsaWS _    _ = Error

getFsa :: Char -> (State -> Char -> State)
getFsa x | isDigit x            = fsaNum
         | isAlpha x            = fsaIdf
         | x `elem` "*^%<>+-/=" = fsaOps
         | x == '(' || x == ')' = fsaBra
         | x == ' '             = fsaWS
         | otherwise = (\y z -> Error)

tokenizer :: String -> [String]
tokenizer xs = allTokens xs []

allTokens :: String -> [String] -> [String]
allTokens "" lst = lst
allTokens (x:xs) lst = allTokens rest (lst ++ [result])
                       where (result, rest) = nextToken (getFsa x) Init "" (x:xs)

nextToken :: (State -> Char -> State) -> State -> String -> String -> (String, String)
nextToken fsa s t "" = (t, "")
nextToken fsa s t (x:xs) = case result of
                            Error -> (t, (x:xs))
                            _     -> nextToken fsa result (t ++ [x]) xs
                           where 
                            result = fsa s x

parse'' :: [String] ->  ((BinTree Char (Either Int Char)), String)
parse'' [] = (Leaf (Left 0), "")
parse'' (y:ys) | isDigit x = (Leaf (Left (read[x]::Int)), xs)
               | isAlpha x = (Leaf (Right x), xs)
               | x == '('  = ((Node op (t0) (t1)), tail r2) 
              where
               (x, xs) = (head y, tail y)
               (t0, r0) = parse'' ys
               (op, r1) = (head r0, tail r0)
               (t1, r2) = parse'' [r1]       
