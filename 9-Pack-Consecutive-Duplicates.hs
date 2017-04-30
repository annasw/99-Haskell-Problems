subPack :: Eq a => [a] -> [[a]] -> [[a]]
subPack [] ls = ls
subPack (x:xs) [] = subPack xs [[x]]
subPack (x:xs) ls
    | x == (head $ last ls) = subPack xs ([i | i <- (init ls)] ++ [(last ls) ++ [head $ last ls]])
    | otherwise = subPack xs (ls ++ [[x]])

pack :: Eq a => [a] -> [[a]]
pack ls = subPack ls []
