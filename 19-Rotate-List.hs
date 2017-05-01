-- Positive rotates left, negative rotates right.
rotate :: [a] -> Int -> [a]
rotate ls n
    | len == 0 = ls
    | n==0 = ls
    | n>0 = [ls!!i | i <- [num..len-1]] ++ [ls!!i | i <- [0..num-1]]
    | n<0 = [ls!!i | i <- [len-num..len-1]] ++ [ls!!i | i <- [0..len-num-1]]
    where num = abs(n) `mod` len
          len = length ls
