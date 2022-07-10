module DNA (toRNA) where

import Data.List

toRNA :: String -> Either Char String
toRNA xs = let (rnaString, error) = partition (`elem` "GCTA") xs
           in if error == ""
              then Right (map toRNAHelper xs)
              else Left (head error)

toRNAHelper :: Char -> Char
toRNAHelper 'G' = 'C'
toRNAHelper 'C' = 'G'
toRNAHelper 'T' = 'A'
toRNAHelper 'A' = 'U'
toRNAHelper x = x