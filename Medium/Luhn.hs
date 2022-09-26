module Luhn (
    isValid,
    ) where

import Data.Char (isSpace, isDigit, digitToInt, intToDigit)

isValid :: String -> Bool
isValid n
  | length n < 2              = False
  | isDigitOrSpace n == False = False
  | n == " 0" || n == "0 "    = False
  | mod sumValue 10 == 0      = True
  | otherwise                 = False
  where num = filter (isDigit) n
        sumValue = getSumValue num


getSumValue :: String -> Int
getSumValue n = sumDigits $ filter (isDigit) luhnNum
  where luhnNum = reverse . uncurryDigits . doubleSndDigits . pairDigits $ reverse n
  
  
isDigitOrSpace :: String -> Bool
isDigitOrSpace = foldr ( \x g-> (&&) (isSpace x || isDigit x) g ) True

pairDigits :: String -> [(Char, Char)]
pairDigits []  = []
pairDigits [x] = [(' ', x)]
pairDigits (x:y:z) = [(x, y)] ++ pairDigits z

doubleSndDigits :: [(Char, Char)] -> [(Char, Char)]
doubleSndDigits xs = map doubleChar xs

uncurryDigits :: [(Char, Char)] -> String
uncurryDigits xs = concatMap  (uncurry ( \a b -> a : b : []) ) xs

sumDigits :: String -> Int
sumDigits = foldr ( \x g -> (+) (digitToInt x) g ) 0 

doubleChar :: (Char, Char) -> (Char, Char)
doubleChar c@(' ', _) = c
doubleChar x
  | doubleInt > 9 = (fst x, intToDigit $ doubleInt - 9)
  | otherwise     = (fst x, intToDigit doubleInt)
  where doubleInt  = (digitToInt $ snd x) * 2
        
