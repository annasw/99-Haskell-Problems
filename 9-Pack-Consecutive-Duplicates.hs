-- a subfunction for the pack method
-- takes a list of chars and (initially) an empty list of strings,
-- returns the packed list of strings
subPack :: [Char] -> [String] -> [String]
subPack [] strs = strs
subPack (x:xs) [] = subPack xs [[x]]
subPack (x:xs) strs
    | x == (head $ last strs) = subPack xs ([i | i <- (init strs)] ++ [(last strs) ++ [head $ last strs]])
    | otherwise = subPack xs (strs ++ [[x]])

pack :: [Char] -> [String]
pack ls = subPack ls []
