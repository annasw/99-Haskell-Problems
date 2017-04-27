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

primeList :: Int -> Int -> [Int]
primeList a b
    | a>b = []
    | isPrime a = [a] ++ primeList (a+1) b
    | otherwise = primeList (a+1) b
    
