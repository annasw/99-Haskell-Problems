checkPhrase :: String -> Bool
checkPhrase [] = False
checkPhrase (x:xs)
    | x `elem` letters = checkRest xs
    | otherwise = False
    where
        letters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
        checkRest [] = True
        checkRest (x:xs)
            | x `elem` letters || x `elem` [(show d)!!0 | d <- [0..9]] = checkRest xs
            | x == '-' = if length xs == 0 || xs!!0=='-' then False else checkRest xs
            
