-- One-indexed according to problem statement
slice :: [a] -> Int -> Int -> [a]
slice ls i k
    | k < i = error "Unacceptable i and k (give i<=k)"
    | k > len ls || i < 1 = error "Unacceptable i and/or k"
    | otherwise = [ls!!(j-1) | j <- [i..k]]
    where len [] = 0
          len (x:xs) = 1 + len xs
          
