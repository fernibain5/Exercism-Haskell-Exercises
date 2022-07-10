module Temperature (tempToC, tempToF) where

{- Implement the function `tempToC` to convert
`  Fahrenheit to Celsius                    -}

tempToC :: Integer -> Float
tempToC temp = (fromIntegral temp - 32) / 1.8 


{- Implement the function `tempToF` to convert
`  Celsius to Fahrenheit                    -}

tempToF :: Float -> Integer
tempToF temp = ceiling $ temp * (9/5) + 32