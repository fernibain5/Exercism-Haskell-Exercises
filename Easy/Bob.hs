module Bob (responseFor) where

import Data.Char
import Data.List

responseFor :: String -> String
responseFor xs
  | dropInitialSpaces xs == ""  ||
    (isInfixOf "\t" xs)             = "Fine. Be that way!"
  | allCapitalLetters && isQuestion 
    && hasSentenceLetters           = "Calm down, I know what I'm doing!"
  | isQuestion                      = "Sure."
  | allCapitalLetters &&
    hasSentenceLetters              = "Whoa, chill out!"
  | otherwise                       = "Whatever."
  where allCapitalLetters = 
          foldr (\x g -> isUpper x && g) True (sentenceNoSpecialChar xs)
        isQuestion = head (dropWhile (== ' ') (reverse xs))  == '?'
        isExclamation = head (dropInitialSpaces (reverse xs)) == '!'
        hasSentenceLetters = sentenceNoSpecialChar xs /= ""
        dropInitialSpaces = dropWhile (== ' ')

sentenceNoSpecialChar :: String -> String
sentenceNoSpecialChar [] = []
sentenceNoSpecialChar (x:xs)
  | isLetter x = x : sentenceNoSpecialChar xs
  | otherwise  = sentenceNoSpecialChar xs
  where isLetter c = (ord c > 96 && ord c < 123 ) 
                     || (ord c > 65 && ord c < 91) 