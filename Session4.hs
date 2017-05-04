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
parse [] = (Leaf 0,[])
parse (x:xs) | isDigit x = (Leaf (read [x]::Int), xs)
             | x == '('  = ((Node op (t0) (t1)), tail r2) 
             where
               (t0, r0) = parse xs
               (op, r1) = (head r0, tail r0)
               (t1, r2) = parse r1

parseTest = showRoseTree $ pp $ fst $ parse "((6*((2^8)-(3/2)))+3)"

--b
parse' :: String -> ((BinTree Char (Either Int Char)), String)
parse' [] = (Leaf (Left 0), [])
parse' (x:xs) | isDigit x = (Leaf (Left (read[x]::Int)), xs)
              | isAlpha x = (Leaf (Right x), xs)
              | x == '('  = ((Node op (t0) (t1)), tail r2) 
              where
               (t0, r0) = parse' xs
               (op, r1) = (head r0, tail r0)
               (t1, r2) = parse' r1
