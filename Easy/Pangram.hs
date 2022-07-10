module Pangram (isPangram) where

import Data.List
import Data.Char

isPangram :: String -> Bool
isPangram [] = False
isPangram (c:cs)
  | sort (nub sentence) == ['a'..'z'] = True
  | otherwise                   = False
  where sentence = sentenceToLower (c:cs)


{- 
Didnt use isLower/isUpper methods in the guard cases to exclude
special characters such as 'Ã'
-}

sentenceToLower :: String -> String
sentenceToLower [] = []
sentenceToLower (x:xs)
  | isLowerCase x  = x : sentenceToLower xs
  | isUpperCase x  = toLower x : sentenceToLower xs
  | otherwise  = sentenceToLower xs
  where isLowerCase c = ord c > 96 && ord c < 123 
        isUpperCase c = ord c > 65 && ord c < 91 