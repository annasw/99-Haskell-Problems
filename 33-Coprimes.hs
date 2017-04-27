-- Same algorithm from 32
myGcd :: Integral a => a -> a -> a
myGcd a b
    | a `mod` b == 0 = b
    | otherwise = gcd b (a `mod` b)

areCoprime :: Integral a => a -> a -> Bool
areCoprime a b = myGcd a b == 1
