module SumOfMultiples (sumOfMultiples) where

import Data.List

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples [0] _ = 0
sumOfMultiples factors limit = 
  sum . (filter  (/= limit)) . nub $
  foldr (\x g -> if x == 0
                 then (++) [0] g
                 else (++) (filter (\a -> a `mod` x == 0) [1..limit]) g)
                      [] factors