dupe :: [a] -> [a]
dupe [] = []
dupe [x] = [x]++[x]
dupe (x:xs) = [x]++[x]++(dupe xs)
