module Golf where

skip :: Int -> [a] -> [a]
skip n xs = map fst (filter (\x -> snd x `mod` n == 0) (zip xs [1..]))

skips :: [a] -> [[a]]
-- skips xs = zipWith ($) (map skip [1..]) $ replicate (length xs) xs
skips = zipWith id (map skip [1..]) . (replicate =<< length) -- 按照大姐姐的建议写成point free形式的（

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:zx)
  | x < y && z < y = y : localMaxima (y:z:zx)
  | otherwise = localMaxima (y:z:zx)
localMaxima _ = []

histogram :: [Integer] -> String
histogram xs = unlines (map (line c) [m+1,m..1]) ++ "==========\n0123456789\n"
  where c = count xs
        m = maximum c
        
line :: [Int] -> Int -> String
line xs n = [if i >= n then '*' else ' ' | i <- xs]

count :: [Integer] -> [Int]
count xs = map (\n -> length $ filter (== n) xs) [0..9]