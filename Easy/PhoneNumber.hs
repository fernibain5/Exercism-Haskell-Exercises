module Phone (number) where

import Data.Char

number :: String -> Maybe String
number xs
  | countDigits == 10 && caseNumber10 = Just onlyNumbers
  | countDigits == 11 && caseNumber11 = Just $ tail onlyNumbers
  | otherwise       = Nothing
  where onlyNumbers       = filter isNumber xs
        countDigits       = length onlyNumbers
        nDigitsAreCorrect = all (\x -> x /= '0' && x /= '1')
        caseNumber10      = nDigitsAreCorrect [head onlyNumbers, onlyNumbers !! 3]
        caseNumber11      = (nDigitsAreCorrect [onlyNumbers !! 1, onlyNumbers !! 4])
                              && head onlyNumbers == '1'