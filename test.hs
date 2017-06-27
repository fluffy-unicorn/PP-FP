import Data.List
--1a

polynome :: [Int] -> Int -> Int
polynome [] _  = 0
polynome (a:as) x = a * (x^n) + polynome as x
                  where n = length as

polynome' :: [Int] -> Int -> Int
polynome' as x = sum $ zipWith (*) as $ map (x^) [n,(n-1)..0]
               where n = length as - 1

polynome'' :: [Int] ->  Int -> Int
polynome'' as x = sum [ as!!(n-i) * (x^i) | i <- [n,(n-1)..0] ]
                where n = length as - 1

--1b
coins :: Int -> [[Int]]
coins value = coins' value 0
coins' :: Int -> Int -> [[Int]]
coins' 0 _ = [[]]
coins' value minVal = [ (i:c) | i <- [1,2,5,10,20,50], i <= value, i >= minVal, c <- coins' (value - i) i ]

--1c
add35 :: Int -> Int
add35 n = sum [ i | i <- [1..(n-1)], i `mod` 3 == 0 || i `mod` 5 == 0]

addks :: [Int] -> Int -> Int
addks ks n = sum [ i | i <- [1..(n-1)], or $ map (\x -> i `mod` x == 0) ks]

--2a
data Tree = Leaf | Node Int Tree Tree deriving Show

--2b
isBalanced :: Tree -> Bool
isBalanced Leaf = True
isBalanced tree = maxDepth tree - minDepth tree <= 1 

maxDepth :: Tree -> Int
maxDepth Leaf = 0
maxDepth (Node _ t1 t2) = (max (maxDepth t1) (maxDepth t2)) + 1

minDepth :: Tree -> Int
minDepth Leaf = 0
minDepth (Node _ t1 t2) = (min (minDepth t1) (minDepth t2)) + 1

--2c
isIsomorphic :: Tree -> Tree -> Bool
isIsomorphic Leaf Leaf = True
isIsomorphic (Node _ t1 t2) (Node _ s1 s2) = isIsomorphic t1 s1 && isIsomorphic t2 s2
isIsomorphic _ _ = False

isIsomorphicPlus :: Int -> Tree -> Tree -> Bool
isIsomorphicPlus _ Leaf Leaf = True
isIsomorphicPlus r (Node n1 t1 t2) (Node n2 s1 s2) = n1 `div` n2 == r && isIsomorphicPlus r t1 t2 && isIsomorphicPlus r t1 t2

--2d
sorted :: Tree -> Bool
sorted Leaf = True
sorted (Node n t1 t2) = maxTree t1 < n && ((minTree t2) > n)

maxTree :: Tree -> Int
maxTree (Node n t1 t2) = case (t1, t2) of
                         (Leaf, Leaf) -> n
                         (Leaf, _) -> max n (maxTree t2)
                         (_, Leaf) -> max n (maxTree t1)
                         _ -> max n $ max (maxTree t1) (maxTree t2)
minTree :: Tree -> Int
minTree Leaf = 0
minTree (Node n t1 t2) = min n $ min (minTree t1) (minTree t2)

--2e
addListToTree :: [Int] -> Tree -> Tree
addListToTree [] t = t
addListToTree (a:as) t =  addListToTree as $ insertIntoTree a t

insertIntoTree :: Int -> Tree -> Tree
insertIntoTree n Leaf = Node n Leaf Leaf
insertIntoTree n (Node x t1 t2) | n < x = Node x (insertIntoTree n t1) t2
                                | n > x = Node x t1 (insertIntoTree n t2)
                                | otherwise = Node x t1 t2
