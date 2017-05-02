range :: Int -> Int -> [Int]
range x y
    | x>y = error "Unacceptable parameters. Give y>=x."
    | otherwise = [i | i <- [x..y]]
