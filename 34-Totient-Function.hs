-- from 32
myGcd :: Integral a => a -> a -> a
myGcd a b
    | a `mod` b == 0 = b
    | otherwise = gcd b (a `mod` b)

-- from 33
areCoprime :: Integral a => a -> a -> Bool
areCoprime a b = myGcd a b == 1

phi :: Integral a => a -> a
phi 1 = 1
phi m = count [ x | x <- [1..m-1], areCoprime m x ]
    where count [] = 0
          count [x] = 1
          count (x:xs) = 1 + (count xs)
          
