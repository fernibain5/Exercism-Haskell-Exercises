module Triangle (rows) where

rows :: Int -> [[Integer]]
rows 0 = []
rows x = f 1 [1]
  where f rowNum prevL  | rowNum == x = prevL : []
                        | otherwise   =  prevL : f (rowNum + 1) newPrevL
                        where newPrevL = [1] ++ listSumPairs prevL ++ [1]

listSumPairs :: [Integer] -> [Integer]
listSumPairs [] = []
listSumPairs [x] = []
listSumPairs [x, y] = (x + y) : []
listSumPairs (x:y:z) = (x + y) : listSumPairs (y:third:rest)
  where third = head z
        rest  = tail z

