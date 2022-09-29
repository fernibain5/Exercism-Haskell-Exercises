module WordCount (wordCount) where

import Data.Char
import Data.List

wordCount :: String -> [(String, Int)]
wordCount xs = map (\x -> (x, length (elemIndices x wrds))) nubbed
  where filterXs = filter (\x -> isSeparator x || isAlphaNum x ) xs
        wrds   =  words $ [if c == ',' then ' ' else c | c <- map toLower filterXs ] 
        nubbed = nub wrds




