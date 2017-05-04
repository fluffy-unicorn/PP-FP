module Session4 where
import FPPrac.Trees
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
isDigit x = x `elem` "0123456789"

isOp :: Char -> Bool
isOp x = x `elem` "+-*/^"

data NT = Ints | Expr | Op

parse :: NT -> String -> ((BinTree a b), String)
parse Ints (x:xs) = (Leaf (read x), xs)
parse Op   (x:xs) = (Leaf x, xs) 
parse Expr (x:xs) | x == '(' = ((Node  
                  where
                    (t0, r0) = parse Expr xs
                    (op, r1) = parse Op r0
                    (t1, r2) = parse Expr r1
