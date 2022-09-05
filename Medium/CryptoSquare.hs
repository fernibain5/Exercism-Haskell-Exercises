module CryptoSquare  where

type Col = Int
type Row = Int
type Spaces = Int

data Rectangle = Rectangle Col Row Spaces

-- encode :: String -> String
-- encode xs
--   | xs == "" = ""
--   | otherwise = seperateSentence xs (Rectangle col row remainderSqrt)
--   where  sqrtOfLength = floor . sqrt . fromIntegral $ length xs
--          remainderSqrt = mod (length xs) sqrtOfLength
--          (row, col) = case remainderSqrt of
--                         0 -> (sqrtOfLength, sqrtOfLength)
--                         otherwise -> (sqrtOfLength, sqrtOfLength + 1)

seperateSentence :: String -> Rectangle -> [String]
seperateSentence xs (Rectangle col row spaces)
  | xs == ""        = []
  | length xs < col = ["((take col xs) :: String ) ++ replicate spaces ' '", ""]

                       
  | otherwise       = map ( )
