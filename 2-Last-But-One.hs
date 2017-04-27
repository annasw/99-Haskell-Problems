lastButOne :: [a] -> a
lastButOne [] = error "Idx out of bounds"
lastButOne [x] = error "Idx out of bounds"
lastButOne (x:y:[]) = x
lastButOne (x:xs) = lastButOne xs
