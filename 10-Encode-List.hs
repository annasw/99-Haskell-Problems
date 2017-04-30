subPack :: Eq a => [a] -> [[a]] -> [[a]]
subPack [] ls = ls
subPack (x:xs) [] = subPack xs [[x]]
subPack (x:xs) ls
    | x == (head $ last ls) = subPack xs ([i | i <- (init ls)] ++ [(last ls) ++ [head $ last ls]])
    | otherwise = subPack xs (ls ++ [[x]])

pack :: Eq a => [a] -> [[a]]
pack ls = subPack ls []

encode :: Eq a => [a] -> [(Int,a)]
encode s = let ls = pack s in zip [len x | x <- ls] [head y | y <- ls]
    where len [] = 0
          len (x:xs) = 1 + len xs
          
