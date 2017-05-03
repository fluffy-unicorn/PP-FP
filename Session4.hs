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

expr :: String -> BinTree a b
expr [] = Leaf Unit
expr [x] = Node (read [x]::Int) (Leaf Unit) (Leaf Unit)
expr (x:xs)     | x `elem` "0123456789" = Node (head xs) (Leaf (read [x]::Int)) (expr (tail xs))
                | x == '(' =  Node (xs!!1) (expr [xs!!2]) (expr xs)
                | otherwise = error "Unknown character"
