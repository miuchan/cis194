module Hw04 where

import Data.List

-- Exercise 1: Wholemeal programming
-- 1.
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even 

-- 2.
fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum 
          . filter even 
          . takeWhile (/= 1) 
          . iterate (\x -> if even x then x `div` 2 else 3 * x + 1) 

-- Exercise 2: Folding with trees
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree [] = Leaf
foldTree xs = Node height 
            (foldTree $ take half xs) 
            (xs !! half)
            (foldTree $ drop (half + 1) xs)
        where 
            len = length xs
            half = len `div` 2
            height = floor (logBase 2 (fromIntegral len)::Double)

-- Exercise 3: More folds!
-- 1. Implement a function
xor :: [Bool] -> Bool
-- xor = odd . length . filter (== True) 
xor = foldl (/=) False . filter (== True)

-- 2. Implement map as a fold.
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

-- 3. (Optional) Implement foldl using foldr. 
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base = foldr (flip f) base . reverse

-- Exercise 4: Finding primes
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\l -> 2 * l + 1) 
                  (map (\(i, j) -> 10 * (i - 1) + j) 
                  (cartProd [1..10] [1..toInteger $ ceiling $ (fromIntegral k :: Double) / 10]) 
                  \\ [i + j + 2 * i * j |j <- [1..k], i <- [1..j], i + j + 2 * i * j <= k ])
  where k = toInteger $ ceiling (((fromIntegral n :: Double) - 2) / 2)

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]
