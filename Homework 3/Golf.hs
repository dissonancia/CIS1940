module Golf where

skips :: [a] -> [[a]]
skips xs = map (`skip` zip [1..] xs) [1..length xs]
  where
    skip n = map snd . filter (\(i, _) -> i `mod` n == 0)

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:zs)
  | x < y && y > z = y : localMaxima (z:zs)
  | otherwise      = localMaxima (y:z:zs)
localMaxima _ = []

histogram :: [Integer] -> String
histogram = undefined