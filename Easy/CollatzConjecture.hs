module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n = go n 0
  where go n count
         | n < 1     = Nothing
         | n == 1    = Just count
         | even n    = go (div n 2) (count + 1)
         | otherwise = go (n * 3 + 1) (count + 1)