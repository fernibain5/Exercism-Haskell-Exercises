module Minesweeper (annotate) where

annotate :: [String] -> [[(a, Char)]]
annotate board = undefined

-- tupledBoard :: [String] -> [[((Int, Int), Char)]]
-- tupledBoard board@(x:xs) = 


mkCoordinates :: Int -> Int -> [[(Int, Int)]]
mkCoordinates row col = take total [ [ (j, i) | i <- [1..row]] | j <- [1..col] ]
  where total = row * col

zippingCoord :: [String] -> [[(Int, Int)]] -> [[((Int, Int), Char)]]
zippingCoord [] _ = []
zippingCoord board@(b:bs) coord@(c:cs) = ((zip c b) : []) ++ zippingCoord bs cs
  