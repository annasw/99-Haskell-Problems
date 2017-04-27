numElts :: (Integral b) => [a] -> b
numElts [] = 0
numElts [x] = 1
numElts (x:xs) = 1 + (numElts xs)
