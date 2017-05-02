module Session1 where
import Data.Char

--Exercise 1
fn :: Float -> Float
fn x = 2 * x ^ 2 + 3 * x - 5

--Exercise 2
code :: Char -> Char
code c = code' 3 c

code' :: Int -> Char -> Char
code' n c 	
  | ord c >= ord 'a' = chr (((ord c - ord 'a' + n) `mod` 26) + ord 'a')
  | otherwise        = chr (((ord c - ord 'A' + n) `mod` 26) + ord 'A')

--Exercise 3
amount :: Float -> Float -> Float -> Float
amount 0 a r = a
amount n a r = (amount (n-1) a r) * (1+r/100)	

--Exercise 4
discr :: Float -> Float -> Float -> Float
discr a b c = b * b - 4 * a * c

root1 :: Float -> Float -> Float -> Float
root1 a b c
	| discr a b c < 0 = error("Negative discriminant")
	| otherwise = ((-1 * b + sqrt(discr a b c)) / (2 * a))

root2 :: Float -> Float -> Float -> Float
root2 a b c
	| discr a b c < 0 = error("Negative discriminant")
	| otherwise = ((-1 * b - sqrt(discr a b c)) / (2 * a))

--Exercise 5
extrX :: Float -> Float -> Float -> Float
extrX a b c = -b / (2*a)

extrY :: Float -> Float -> Float -> Float
extrY a b c = a * (extrX a b c ** 2) + b * extrX a b c + c

--Exercise 6
mylength:: [a] -> Int
mylength [] = 0
mylength (x:xs) = mylength xs + 1 

mysum:: (Num a) => [a] -> a
mysum [] = 0
mysum (x:xs) = mysum xs + x

myreverse:: [a] -> [a]
myreverse [] = []
myreverse (x:xs) = myreverse xs ++ [x]

mytake:: [a] -> Int -> [a]
mytake [] n = []
mytake _ 0  = []
mytake (x:xs) n = [x] ++ mytake xs (n-1) 

myelem:: (Eq a) => [a] -> a -> Bool
myelem [] a = False
myelem (x:xs) a | x == a    = True
                | otherwise = myelem xs a

myconcat:: [a] -> [a] -> [a]
myconcat xs [] = xs
myconcat xs (y:ys) = myconcat (xs ++ [y]) ys
 
mymaximum:: (Num a, Ord a) => [a] -> a
mymaximum [] = -1
mymaximum (x:xs) | x > mymaximum xs = x
                 | otherwise        = mymaximum xs

myzip:: [a] -> [a] -> [(a,a)]
myzip xs [] = []
myzip [] ys = []
myzip (x:xs) (y:ys) = [(x,y)] ++ myzip xs ys

--Exercise 7
r:: (Num a) => a -> a -> [a]
r a d = [a] ++ r (a+d) d

r1:: (Num a) => a -> a -> Int -> a
r1 a d 0 = a
r1 a d n = r1 (a+d) d (n-1)

r2:: (Num a) => a -> a -> Int -> Int -> a
r2 a d i j = mysum (map (r1 a d) [i..j])

--Exercise 8
allEqual:: (Eq a) => [a] -> Bool
allEqual [] = True
allEqual [x] = True
allEqual (x:x':xs) | x==x'     = allEqual (x':xs)
                   | otherwise = False

isAS:: (Num a, Eq a) => [a] -> Bool
isAS xs = allEqual (differences xs)

differences:: (Num a, Eq a) => [a] -> [a]
differences [] = []
differences [x] = []
differences (x:x':xs) = [x-x'] ++ differences (x':xs)

--Exercise 9
rowLengthEqual :: [[a]] -> Bool
rowLengthEqual xs = allEqual (map mylength xs)

rowSum :: (Num a) => [[a]] -> [a]
rowSum xs = map mysum xs

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose [x] = transposeRow x
transpose (x:xs) = zipWith (++) (transposeRow x) (transpose xs)

transposeRow :: [a] -> [[a]]
transposeRow [] = []
transposeRow (x:xs) = [[x]] ++ transposeRow xs

colSum :: (Num a) => [[a]] -> [a]
colSum xs = rowSum (transpose xs)
