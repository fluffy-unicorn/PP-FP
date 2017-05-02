module Session3 where
import FPPrac.Trees

--1a
data Tree1a = Leaf1a Int | Node1a Int Tree1a Tree1a
--data RoseTree = RoseNode String [RoseTree]
pp1a :: Tree1a -> RoseTree
pp1a (Leaf1a x) = RoseNode (show x) []
pp1a (Node1a x t1 t2) = RoseNode (show x) [ pp1a t1, pp1a t2 ]

--b
data Tree1b = Leaf1b (Int, Int) | Node1b (Int, Int) Tree1b Tree1b
pp1b :: Tree1b -> RoseTree
pp1b (Leaf1b x) = RoseNode (show x) []
pp1b (Node1b x t1 t2) = RoseNode (show x) [ pp1b t1, pp1b t2 ]

--c 
data Tree1c = Leaf1c Int | Node1c Tree1c Tree1c
pp1c :: Tree1c -> RoseTree
pp1c (Leaf1c x) = RoseNode (show x) []
pp1c (Node1c t1 t2) = RoseNode "" [ pp1c t1, pp1c t2 ]

--d
data Tree1d = Leaf1d (Int, Int) | Node1d [ Tree1d ]
pp1d :: Tree1d -> RoseTree
pp1d (Leaf1d x) = RoseNode (show x) []
pp1d (Node1d ts) = RoseNode "" (map pp1d ts)

tr1a = Node1a 10 (Node1a 20 (Leaf1a 30) (Leaf1a 40)) (Leaf1a 50)
tr1b = Node1b (1,2) (Node1b (2,3) (Leaf1b (3,4)) (Leaf1b (4,5))) (Leaf1b (5,6))
tr1c = Node1c (Node1c (Node1c (Leaf1c 10) (Leaf1c 20)) (Leaf1c 30)) (Leaf1c 40)
tr1d = Node1d [(Node1d [ Leaf1d (1,1) ]),(Node1d [ Leaf1d (1,2) ]),(Node1d [ Leaf1d (1,3) ])]

--2a
treeAdd :: Tree1a -> Int -> Tree1a
treeAdd (Leaf1a n) x = Leaf1a (n+x)
treeAdd (Node1a n t1 t2) x = Node1a (n+x) (treeAdd t1 x) (treeAdd t2 x)

--b
treeSquare :: Tree1a -> Tree1a
treeSquare (Leaf1a n) = Leaf1a (n*n)
treeSquare (Node1a n t1 t2) = Node1a (n*n) (treeSquare t1) (treeSquare t2)

--c
mapTree :: (Int -> Int) -> Tree1a -> Tree1a
mapTree f (Leaf1a n) = Leaf1a (f n)
mapTree f (Node1a n t1 t2) = Node1a (f n) (mapTree f t1) (mapTree f t2)

treeAdd' :: Tree1a -> Int -> Tree1a
treeAdd' t x = mapTree (+x) t

treeSquare' :: Tree1a -> Tree1a
treeSquare' t = mapTree (^2) t

--d
addNode :: Tree1b -> Tree1a
addNode (Leaf1b (x,y)) = Leaf1a (x + y)
addNode (Node1b (x,y) t1 t2) = Node1a (x+y) (addNode t1) (addNode t2)

--e
mapTree' :: ((Int, Int) -> Int) -> Tree1b -> Tree1a
mapTree' f (Leaf1b xy) = Leaf1a (f xy)
mapTree' f (Node1b xy t1 t2) = Node1a (f xy) (mapTree' f t1) (mapTree' f t2)

mtTest1 = mapTree' (\(x,y) -> x+y) tr1b
mtTest2 = mapTree' (\(x,y) -> x*y) tr1b

--3a
binMirror :: Tree1a -> Tree1a
binMirror (Leaf1a n) = Leaf1a n
binMirror (Node1a n t1 t2) = Node1a n (binMirror t2) (binMirror t1)

--b
binMirror' :: Tree1d -> Tree1d
binMirror' (Leaf1d (x,y)) = Leaf1d (y,x)
binMirror' (Node1d ts) = Node1d (map binMirror' (reverse ts))

--4
data Tree4 = Leaf4 | Node4 Int Tree4 Tree4 deriving (Eq, Show)
--a
insertTree :: Tree4 -> Int -> Tree4
insertTree Leaf4 n = Node4 n Leaf4 Leaf4
insertTree (Node4 n1 t1 t2) n | n <= n1   = Node4 n1 (insertTree t1 n) t2
                              | otherwise = Node4 n1 t1 (insertTree t2 n)

--b
makeTree' :: [Int] -> Tree4 -> Tree4
makeTree' [] t4 = Leaf4
makeTree' xs t4 = (insertTree (makeTree' (init xs) t4) (last xs)) 

makeTree :: [Int] -> Tree4
makeTree xs = makeTree' xs Leaf4

makeTree'' :: [Int] -> Tree4
makeTree'' xs = foldl insertTree Leaf4 xs

--c
makeList :: Tree4 -> [Int]
makeList Leaf4 = []
makeList (Node4 n t1 t2) = makeList t1 ++ [n] ++ makeList t2

--d
treeSort :: [Int] -> [Int]
treeSort xs = makeList $ makeTree'' xs

--e
sortTree :: Tree4 -> Tree4
sortTree t4 = makeTree'' $ makeList t4

--5
subtreeAt :: Tree4 -> Int -> Tree4
subtreeAt Leaf4 _ = error "No Subtree Found"
subtreeAt (Node4 n t1 t2) x | x == n    = (Node4 n t1 t2)
                            | x < n     = subtreeAt t1 x
                            | otherwise = subtreeAt t2 x

--6
cutOffAt :: Tree1a -> Int -> Tree1a
cutOffAt (Leaf1a n)       _ = Leaf1a n
cutOffAt (Node1a n _ _)   0 = Leaf1a n 
cutOffAt (Node1a n t1 t2) x = Node1a n (cutOffAt t1 (x-1)) (cutOffAt t2 (x-1))

--7a
replace :: Tree1a -> String -> Int -> Tree1a
replace (Leaf1a _) [] x = Leaf1a x
replace (Leaf1a x) _  _ = Leaf1a x
replace (Node1a _ t1 t2) [] x = Node1a x t1 t2
replace (Node1a n t1 t2) (c:cs) x | c == 'l' = Node1a n (replace t1 cs x) t2
                                  | c == 'r' = Node1a n t1 (replace t2 cs x)
                                  | otherwise = error "Path Contains Bad Character"
--b
subtree :: Tree1a -> String -> Tree1a
subtree (Leaf1a x) [] = Leaf1a x
subtree (Leaf1a _) _  = error "Path Is Too Long"
subtree (Node1a n t1 t2) []   = Node1a n t1 t2
subtree (Node1a _ t1 t2) (c:cs) | c == 'l' = subtree t1 cs
                                | c == 'r' = subtree t2 cs
                                | otherwise = error "Path Contains Bad Character"


 
