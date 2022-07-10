module LuciansLusciousLasagna (elapsedTimeInMinutes, expectedMinutesInOven, preparationTimeInMinutes) where

expectedMinutesInOven :: Integer
expectedMinutesInOven = 40

preparationTimeInMinutes :: Num a => a -> a
preparationTimeInMinutes x = x * 2

elapsedTimeInMinutes :: Num a => a -> a
elapsedTimeInMinutes x = preparationTimeInMinutes x + 20