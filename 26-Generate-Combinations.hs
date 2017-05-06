combinations :: Eq a => Int -> [a] -> [[a]]
combinations 0 _ = []
combinations _ [] = []
combinations 1 ls = [[i] | i <- ls]
combinations n (x:xs)
    | n > length (x:xs) = []
    | otherwise = [x:ls | ls <- combinations (n-1) xs, ls /= []] ++ combinations n xs
