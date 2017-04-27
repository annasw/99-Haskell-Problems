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
    | x <= 2 || x `mod` 2 /= 0 = error "Give x > 2 and even please."
    | otherwise = findFactors 3 x
    where findFactors n x
              | n>x = error ("The Goldbach Conjecture is false for x=" ++ show x)
              | isPrime n && isPrime (x-n) = (n, x-n)
              | otherwise = findFactors (n+2) x
              
