module Acronym (abbreviate) where

import Data.Char

abbreviate :: String -> String
abbreviate xs
  | xs == [] = []
  | all isUpper word = [head word] ++ abbreviate newSentence
  | length (capitalLetters word) > 1 = 
      capitalLetters word ++ abbreviate newSentence
  | otherwise = [toUpper (head word)] ++ abbreviate newSentence
  where deleteNonLetters = 
          dropWhile (\x -> not (isLetter x)) xs
        word             = takeWhile (\x -> isLetter x || x == '\'') deleteNonLetters
        capitalLetters   = filter (isUpper)
        newSentence      = drop (length word) deleteNonLetters