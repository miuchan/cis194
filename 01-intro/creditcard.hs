{-# LANGUAGE ViewPatterns #-}

module CreditCard where

toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev x
  | x < 0 = []
  | otherwise = x `mod` 10 : toDigitsRev (x `div` 10)

toDigits :: Integer -> [Integer]
toDigits x = reverse (toDigitsRev x)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther (reverse -> (x : y : zs)) = 
  doubleEveryOther (reverse zs) ++ [2 * y, x]
doubleEveryOther ([x]) = [x]
doubleEveryOther _ = []

sumDigits :: [Integer] -> Integer
sumDigits = sum . map (sum . toDigits)

validate :: Integer -> Bool
validate x = (sumDigits . doubleEveryOther . toDigits) x `mod` 10 == 0