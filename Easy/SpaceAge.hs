module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune
            deriving (Eq, Show)

ageOn :: Planet -> Float -> Float
ageOn planet seconds
  |  planet == Mercury = earthYears / 0.2408467 
  |  planet == Venus   = earthYears / 0.61519726 
  |  planet == Earth   = earthYears
  |  planet == Mars    = earthYears / 1.8808158 
  |  planet == Jupiter = earthYears / 11.862615 
  |  planet == Saturn  = earthYears / 29.447498 
  |  planet == Uranus  = earthYears / 84.016846 
  |  planet == Neptune = earthYears / 164.79132 
  | otherwise          = 0.0
  where earthYears = seconds / 31557600 