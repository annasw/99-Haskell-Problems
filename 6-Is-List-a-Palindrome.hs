isPalindrome :: (Ord a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome xs = if head xs == last xs then recur else False
    where recur = isPalindrome $ tail $ init xs
