module Test (annotate) where


import Data.Char (intToDigit)

annotate :: [String] -> [String]
annotate m = zipWith (zipWith toChar) mines adj
  where
    mines = (map . map) (== '*') m
    adj = smooth . (map . map $ fromEnum) $ mines
    toChar True _  = '*'
    toChar False 0 = ' '
    toChar False n = intToDigit n

smooth :: [[Int]] -> [[Int]]
smooth = map (trips add3 0) . trips (zipWith3 add3) (repeat 0)
  where
    add3 a b c = a + b + c

trips :: (a -> a -> a -> b) -> a -> [a] -> [b]
trips f border = go . (++ [border]) . (border :)
  where
    go l@(a:b:c:_) = f a b c : go (tail l)
    go _           = []