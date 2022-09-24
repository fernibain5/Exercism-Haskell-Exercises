module Clock (addDelta, fromHourMin, toString) where

type Hours = Int
type Mins  = Int

data Clock = Clock Hours Mins
  deriving (Eq, Show)

fromHourMin :: Int -> Int -> Clock
fromHourMin h m = Clock hours mins
  where isMinNeg = if (m < 0) && (mod m 60 /= 0 )
                   then 1 
                   else 0
        hours    = absHour (h + (quot m 60) - isMinNeg) 
        mins     = mod m 60

absHour :: Hours -> Hours
absHour h
  | h >= 24 || h < 0 = mod h 24
  | otherwise       = h 


toString :: Clock -> String
toString (Clock h m) = toStringDigs h ++ ":" ++ toStringDigs m

toStringDigs :: Int -> String
toStringDigs i
  | i >= 0 && i < 10 = "0" ++ show i
  | otherwise        = show i

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour min (Clock h m) = fromHourMin (hour + h) (min + m)
