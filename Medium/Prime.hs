module Prime (nth) where
nth :: Int -> Maybe Integer
nth 0 = Nothing
nth n = Just . fromIntegral . last $ take n primes


primes :: [Int]
primes = 2 : [x | x <- [3..], isPrime x]

isPrime :: Int -> Bool
isPrime x = all (\p -> mod x p > 0) (factorsToTry x)
  where factorsToTry x = takeWhile (\p -> p*p <= x) primes
