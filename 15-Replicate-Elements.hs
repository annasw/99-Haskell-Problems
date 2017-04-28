replic :: [a] -> Int -> [a]
replic [] n = []
replic [x] n = [ a | a <- (repl x n) ]
    where repl x 0 = []
          repl x n = [x]++(repl x (n-1))
replic (x:xs) n = replic [x] n ++ replic xs n
