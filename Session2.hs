module Session2 where
import Data.Char
import Data.List

--exercise 1
myfilter :: (a -> Bool) -> [a] -> [a]
myfilter f [] = []
myfilter f (x:xs) | f x = [x] ++ myfilter f xs
                  | otherwise = myfilter f xs

myfoldl ::(a -> b -> a) -> a -> [b] -> a
myfoldl f z [] = z
myfoldl f z (x:xs) = (myfoldl f z xs) `f` x 

myfoldr ::(a -> b -> b) -> b -> [a] -> b
myfoldr f z [] = z
myfoldr f z (x:xs) = x `f` (myfoldr f z xs)

myZipWith:: (a-> b -> c) -> [a] -> [b] -> [c]
myZipWith f [] (ys) = []
myZipWith f (xs) [] = []
myZipWith f (x:xs) (y:ys) = [f x y] ++ myZipWith f xs ys

--exercise 2
--a
database :: [(String, Int, String, String)]
database = [("Mario", 20, "Male", "Enschede"), ("Luigi", 32, "Male", "Hengelo"), ("Peach", 35, "Female", "Another castle"), ("Daisy", 31, "Female", "Twekkelerveld")]
--b
name :: (String, Int, String, String) -> String
name (x,_,_,_) = x

age :: (String, Int, String, String) -> Int
age (_,x,_,_) = x

sex :: (String, Int, String, String) -> String
sex (_,_,x,_) = x

por :: (String, Int, String, String) -> String
por (_,_,_,x) = x

--c
--recursion
increaseAge :: [(String, Int, String, String)] -> Int -> [(String, Int, String, String)]
increaseAge [] n = []
increaseAge ((a,b,c,d):xs) n = [(a,b+n,c,d)] ++ increaseAge xs n

--list comprehension
increaseAge' :: [(String, Int, String, String)] -> Int -> [(String, Int, String, String)]
increaseAge' db n= [(a,b+n,c,d) | (a,b,c,d) <- db]

--Higher order functions
increaseAge'' :: [(String, Int, String, String)] -> Int -> [(String, Int, String, String)]
increaseAge'' db n = map (h n) db

h :: Int -> (String, Int, String, String) -> (String, Int, String, String)
h n (a,b,c,d) = (a,b+n,c,d)

--c
--recursion
women3040 :: [(String, Int, String, String)] -> [String]
women3040 [] = []
women3040 ((a,b,c,d):xs) | b > 30 && b < 40 && c == "Female" = [(a)] ++ women3040 xs
                         | otherwise = women3040 xs

--d
women3040' :: [(String, Int, String, String)] -> [String]
women3040' db = [a | (a,b,c,d) <- db, b > 30, b < 40, c == "Female" ]

women3040'' :: [(String, Int, String, String)] -> [String] 
women3040'' db = map name (filter milffilter db)

milffilter :: (String, Int, String, String) -> Bool
milffilter (a,b,c,d) = b > 30 && b < 40 && c == "Female" 

--e
ageofperson :: [(String, Int, String, String)] -> String -> [Int]
ageofperson db n = [b | (a,b,c,d) <-db, (map toUpper a) == (map toUpper n)]

--f
sortDatabase :: [(String, Int, String, String)] -> [(String, Int, String, String)]
sortDatabase db = map swapAgeAndName (sort [(b,a,c,d) | (a,b,c,d) <- db])

swapAgeAndName :: ( Int, String, String, String) -> (String, Int, String, String)
swapAgeAndName (b,a,c,d) = (a,b,c,d) 

--Exercise 3
--a
sieve :: [Int] -> [Int]
sieve (x:xs) = x : [n | n <- sieve xs, n `rem` x > 0]

--1
prime :: Int -> Bool
prime n = (last $ takeWhile (<=n) [ p | p <- sieve [2..]]) == n

--2
firstNPrimes :: Int -> [Int]
firstNPrimes n = take n [p | p <- [2..], prime' p]

--3
primesUnderN :: Int -> [Int]
primesUnderN n = [p | p <- [2..n], prime' p]

--b
dividers :: Int -> [Int]
dividers m = [d | d <- [1..m], m `mod` d == 0]

prime' :: Int -> Bool
prime' n = length (dividers n) == 2

--Exercise 4
isInt :: RealFrac t => t -> Bool
isInt x = x == fromInteger(round x)

gcd' :: (RealFrac t, Integral i) => t -> t -> i
gcd' a b = gcd (round a) (round b)

pyth :: Int -> [(Int, Int, Int)]
pyth n = [(a,b,c) | a <- [1..n], b <- [1..n], c <- [1..n], a*a + b*b == c*c] 

pyth' :: (Floating a, Enum a, RealFrac a) => [(a,a,a)]
pyth' = [(b, a, c) | a <- [1..], b <- [1..a], c <- [sqrt(a*a+b*b)], isInt c, gcd' a b == 1]

--Exercise 5
increasing :: Ord t => [t] -> Bool
increasing [] = True
increasing [x] = True
increasing (x1:x2:xs) = x1 < x2 && increasing (x2:xs)

avg :: (Fractional a, Ord a) => [a] -> a
avg [] = 0
avg [x] = x
avg (x:xs) = (x + (avg xs * xsLength)) / (xsLength + 1)
             where xsLength = fromIntegral(length xs)
 
weakIncr :: (Fractional t, Ord t) => [t] -> Bool
weakIncr [] = True
weakIncr [x] = True
weakIncr xs = (avg (init xs) < last xs) && (weakIncr $ init xs)

--Exercise 6
sublist :: Ord t => [t] -> [t] -> Bool
sublist [] ys = True
sublist xs [] = False
sublist (x:xs) (y:ys) = (x == y && sublist xs ys) || sublist (x:xs) ys
-- TODO

partSL :: Ord t => [t] -> [t] -> Bool
partSL [] ys = True
partSL xs [] = False
partSL (x:xs) (y:ys) = (x == y && partSL xs ys) || partSL (x:xs) ys

--Exercise 7
--a
bubble :: Ord t => [t] -> [t]
bubble [] = []
bubble [x] = [x]
bubble (x:x':xs) = [min x x'] ++ bubble ((max x x'):xs)

bsort :: Ord t => [t] -> [t]
bsort [] = []
bsort [x] = [x]
bsort xs = bsort (init xs) ++ [last $ bubble xs]
-- TODO optimization
--b
minlist :: Ord t => [t] -> t
minlist [x] = x
minlist (x:xs) = min x (minlist xs)

maxlist :: Ord t => [t] -> t
maxlist [x] = x
maxlist (x:xs) = max x (maxlist xs)

mmsort :: Ord t => [t] -> [t]
mmsort [] = []
mmsort [x] = [x]
mmsort xs = [mn] ++ mmsort (xs \\ [mn, mx]) ++ [mx]
            where mn = minlist xs
                  mx = maxlist xs
--c
ins :: Ord t => [t] -> t -> [t]
ins [] y = [y]
ins (x:xs) y | y < x = (y : x : xs)
             | otherwise = (x : (ins xs y))

isort :: Ord t => [t] -> [t]
isort xs = foldl ins [] xs
--d
merge :: Ord t => [t] -> [t] -> [t]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x < y     = [x] ++ merge xs (y:ys)
                    | otherwise = [y] ++ merge (x:xs) ys

msort :: Ord t => [t] -> [t]
msort [] = []
msort [x] = [x]
msort xs = merge (msort $ fst $ halfLists) (msort $ snd $ halfLists)
           where halfLists = splitAt ((length xs + 1) `div` 2) xs
--e
qsort :: Ord t => [t] -> [t]
qsort [] = []
qsort (x:xs) = qsort [ a | a <- xs, a <= x] ++ [x] ++ qsort [ b | b <- xs, b > x]
