module Robot
    ( Bearing(East,North,South,West)
    , bearing
    , coordinates
    , mkRobot
    , move
    ) where

import Data.Char

data Bearing = North
             | East
             | South
             | West
             deriving (Eq, Show)
type Position = (Integer, Integer)

data Robot = Robot Bearing Position deriving (Eq, Show)

bearing :: Robot -> Bearing
bearing (Robot b _) = b

coordinates :: Robot -> Position
coordinates (Robot _ p) = p

mkRobot :: Bearing -> Position -> Robot
mkRobot = Robot

validateInstructions :: String -> Bool
validateInstructions = foldr (\x g -> (&&) (toUpper x == 'A' || toUpper x ==
                                         'L' || toUpper x == 'R') g ) True

turnDirection :: Bearing -> Char -> Bearing
turnDirection b c
  | isIt North && isLeft = West
  | isIt North           = East
  | isIt West && isLeft  = South
  | isIt West            = North
  | isIt South && isLeft = East
  | isIt South           = West
  | isIt East && isLeft  = North
  | isIt East            = South
  | otherwise            = undefined
  where isIt x = b == x
        isLeft = c == 'L'

advanceOnce :: Bearing -> Position -> Position
advanceOnce b (x, y)
  | isIt North = (x, y + 1)
  | isIt South = (x, y - 1)
  | isIt East  = (x + 1, y)
  | isIt West  = (x - 1, y)
  | otherwise  = (x, y)
  where isIt x = b == x

move :: Robot -> String -> Robot
move (Robot b (x, y)) instr =
  if not (validateInstructions instr)
  then Robot b (x, y)
  else go b (x, y) instr
       where go b (x, y) xs
              | null xs       = Robot b (x, y)
              | fstChar == 'A' = go b (advanceOnce b (x, y)) (tail xs)
              | otherwise      = go (turnDirection b fstChar) (x, y) (tail xs)
               where fstChar = toUpper (head xs)