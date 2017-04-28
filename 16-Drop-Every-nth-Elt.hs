dropEvery :: [a] -> Int -> [a]
dropEvery [] n = []
dropEvery [x] n = if n==1 then [] else [x]
dropEvery ls n = dropLoop ls n 1
    where dropLoop [] n count = []
          dropLoop [x] n count = if count==n then [] else [x]
          dropLoop (x:xs) n count
              | count == n = dropLoop xs n 1
              | otherwise = [x] ++ dropLoop xs n (count+1)
