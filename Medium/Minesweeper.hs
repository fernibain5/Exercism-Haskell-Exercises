module Minesweeper  where

import Data.Maybe


type Col = Int
type Row = Int

data Rectangle = Rectangle Row Col deriving Show

annotate :: [String] -> [String]
annotate [ "" ] = [ "" ]
annotate board = fmap concat $ (map . map) (\x -> if snd x /= '*'
                                                  then case getIntBombs x of
                                                         0 -> take 1 $ [snd x]
                                                         otherwise -> show $ getIntBombs x
                                                  else "*" ) boardZipped                                           
    where getRect = Rectangle (length $ head board) (length board)
          boardZipped = zippingBoard board (mkCoordinates getRect)
          concatBoard = concat $ boardZipped
          getIntBombs x = totalBombsCoord concatBoard (bombCoord getRect (fst x))


mkCoordinates :: Rectangle-> [[(Int, Int)]]
mkCoordinates (Rectangle row col) = take total [ [ (rw, cl) | rw <- [1..row]] | cl <- [1..col] ]
  where total = row * col

zippingBoard :: [String] -> [[(Int, Int)]] -> [[((Int, Int), Char)]]
zippingBoard [] _ = []
zippingBoard board@(b:bs) coord@(c:cs) = ((zip c b) : []) ++ zippingBoard bs cs

bombCoord :: Rectangle -> (Int, Int) -> [(Int, Int)]
bombCoord (Rectangle row col) c@((a, b)) = 
  [(y, x) | y <- [1..row],
            x <- [1..col],
            y >= a - 1 && y < a + 2 && x >= b - 1 && x < b + 2                                                                                                                   
            ]

totalBombsCoord :: [((Int, Int), Char)] -> [(Int, Int)] -> Int
totalBombsCoord board lCoord = length . filter (== '*') . catMaybes $ map ( (flip lookup) board ) lCoord  


