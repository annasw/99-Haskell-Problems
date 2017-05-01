data Code a = Single a | Mult Int a deriving Show

encodeHelper :: Eq a => [a] -> [Code a] -> [Code a]
encodeHelper [] ls = ls
encodeHelper (x:xs) [] = encodeHelper xs [Single x]
encodeHelper (x:xs) (ls)
    | x == (lastType $ last ls) = encodeHelper xs (init ls ++ [Mult ((lastCount $ last ls)+1) x])
    | otherwise = encodeHelper xs (ls ++ [Single x])
    where lastType (Single x) = x
          lastType (Mult n x) = x
          lastCount (Single x) = 1
          lastCount (Mult n x) = n

encodeDirect :: Eq a => [a] -> [Code a]
encodeDirect ls = encodeHelper ls []
