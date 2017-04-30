compress :: Ord a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:y:ys)
    | x == y = compress(y:ys)
    | otherwise = [x] ++ compress (y:ys)
