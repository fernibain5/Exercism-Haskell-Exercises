{-# LANGUAGE ParallelListComp #-}

module Queens (
    boardString,
     canAttack
     ) where

import Data.List (intersperse)


boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString white black = 
  unlines [ unwords [ board i j | j <- [0..7]] | i <- [0..7]]
  where board i j  | Just (i, j) == white = "W"
                   | Just (i, j) == black = "B"
                   | otherwise            = "_"

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack white black = any (== black) whiteMoves
  where whiteMoves = possibleMoves white

possibleMoves :: (Int, Int) -> [(Int, Int)]
possibleMoves (r, c) = let vertical   = [ (i, c) | i <- [0..7], (i, c) /= (r, c) ]
                           horizontal = [ (r, j) | j <- [0..7], (r, j) /= (r, c) ]
                           diagonal   = [ (i, j) | i <- [0..7], j <- [0..7], i - j == r - c, (i, j) /= (r, c) ]
                           diagonal'  = [ (i, j) | i <- [0..7], j <- [0..7], i + j == r + c, (i, j) /= (r, c) ]
                       in  vertical ++ horizontal ++ diagonal ++ diagonal' 


