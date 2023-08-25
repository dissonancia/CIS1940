module Validate where

toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = map (read . (:[])) $ show n

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = zipWith (*) (cycle l) xs
  where l = if even $ length xs then [2,1] else [1,2]

sumDigits :: [Integer] -> Integer
sumDigits = sum . map (sum . toDigits)

validate :: Integer -> Bool
validate = (==0) . (`mod` 10) . sumDigits . doubleEveryOther . toDigits
