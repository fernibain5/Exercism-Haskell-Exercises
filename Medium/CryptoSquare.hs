module CryptoSquare  where

import Data.Char

type Col = Int
type Row = Int

data Rectangle = Rectangle Col Row deriving Show

encode :: String -> String
encode xs
  | xs == "" = ""
  | otherwise = encodeAux $ separateSentence filteredSent rect
  where lettersAndNum  = filter (\x -> isLetter x || isNumber x) xs
        rect = getRectangle lettersAndNum
        filteredSent = filterSentence lettersAndNum rect

encodeAux :: [String] -> String
encodeAux xs
  | xs == []              = ""
  | length (head xs) == 1 = takeHeads
  | otherwise             = takeHeads ++ " " ++ encodeAux (map (drop 1) xs)
  where takeHeads = concatMap (take 1) xs

-- It necessary to pass as an argument a sentence with only letters
getRectangle :: String -> Rectangle
getRectangle xs
  | xs  == "" = (Rectangle 0 0)
  | otherwise = (Rectangle col row)
  where sqrtCode = sqrt . fromIntegral $ length xs
        roundSqrt = round sqrtCode
        decimalSqrt = snd (properFraction sqrtCode)
        (row, col) = case 0.0 == decimalSqrt || decimalSqrt > 0.5 of
                     True  -> (roundSqrt, roundSqrt)
                     False -> (roundSqrt, roundSqrt + 1)

addSpaces :: Int -> String -> String
addSpaces s xs = xs ++ replicate s ' '

filterSentence :: String -> Rectangle -> String
filterSentence "" _ = ""
filterSentence xs (Rectangle col row) =   let spaces    = col * row - (length xs)
                                              onlyLower = map toLower xs  
                                          in  addSpaces spaces onlyLower


separateSentence :: String -> Rectangle -> [String]
separateSentence "" _                     = []
separateSentence xs rect@(Rectangle col _) = take col xs : separateSentence (drop col xs) rect
                               


                       
