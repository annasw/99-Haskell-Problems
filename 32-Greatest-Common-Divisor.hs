myGcd :: Integral a => a -> a -> a
myGcd a b
    | a `mod` b == 0 = b
    | otherwise = gcd b (a `mod` b)
