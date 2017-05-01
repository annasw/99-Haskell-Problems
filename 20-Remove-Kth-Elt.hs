-- N.B.: The question asks for 1-indexing, but my solution is 0-indexed (on principle).
removeAt :: [a] -> Int -> (a, [a])
removeAt ls idx
    | idx >= length ls = error "Index out of range."
    | otherwise = (ls!!idx, [ls!!i | i <- [0..idx-1]]++[ls!!i | i <- [idx+1..(length ls)-1]])
