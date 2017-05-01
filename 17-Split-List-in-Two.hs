splitHelper :: [a] -> Int -> [a] -> [a]
splitHelper ls 0 result = result
splitHelper [] n result = error "N too large."
splitHelper (x:xs) n result = splitHelper xs (n-1) (result++[x])

split :: [a] -> Int -> ([a],[a])
split ls 0 = ([],ls)
split [] n = error "N too large."
split ls n = (splitHelper ls n [], remainder ls n)
    where remainder ls 0 = ls
          remainder (x:xs) n = remainder xs (n-1)
