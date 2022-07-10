module Strain (keep, discard) where

discard :: (a -> Bool) -> [a] -> [a]
discard _ [] = []
discard p xs = concatMap (\x -> if p x == False
                          then [x]
                          else []) xs

keep :: (a -> Bool) -> [a] -> [a]
keep p xs = concatMap (\x -> if p x == True
                          then [x]
                          else []) xs
