isPrime :: Int -> Bool
isPrime n
    | n <= 1 = False
    | n == 2 || n==3 || n==5 = True
    | n `mod` 2 == 0 = False
    | otherwise = checkDivision n divisors
    where endPoint = let f = ((floor $ sqrt $ fromIntegral n)+1) in
                     if f `mod` 2 == 0 then f+1 else f
          divisors = [3,5..endPoint]
          checkDivision n [] = True
          checkDivision n [x] = n `mod` x /= 0
          checkDivision n (x:xs)
              | n `mod` x == 0 = False
              | otherwise = checkDivision n xs

goldbach :: Int -> (Int, Int)
goldbach x
    | x < 2 || x `mod` 2 /= 0 = error "Give x > 2 and even please."
    | x == 4 = (2, 2)
    | otherwise = findFactors 3 x
    where findFactors n x
              | n>x = error ("The Goldbach Conjecture is false for x=" ++ show x)
              | isPrime n && isPrime (x-n) = (n, x-n)
              | otherwise = findFactors (n+2) x

goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList lo hi
    | lo > hi = []
    | lo <= 2 = goldbachList 4 hi
    | lo `mod` 2 == 0 = [goldbach lo] ++ (goldbachList (lo+2) hi)
    | otherwise = goldbachList (lo+1) hi

goldbachList' :: Int -> Int -> Int -> [(Int, Int)]
goldbachList' lo hi minPrime = [ e | e <- goldbachList lo hi, fst e >= minPrime, snd e >= minPrime ]
