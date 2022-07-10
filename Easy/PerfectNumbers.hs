module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n 
  | n < 1 = Nothing
  | aliquoutSum == n = Just Perfect
  | aliquoutSum > n  = Just Abundant
  | otherwise        = Just Deficient
  where aliquoutSum = sum [ x  | x <- [1..n] , mod n x == 0, x /= n ]