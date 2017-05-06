lsortA :: [[a]] -> [[a]]
lsortA [] = []
lsortA (x:xs) = (lsortA $ filter (\ls -> length ls <= length x) xs) ++ [x] ++ (lsortA $ filter (\ls -> length ls > length x) xs)
