-- 0-indexed on principle.
-- Inserting into an empty list (at 0) returns a list with only the inserted elt.
insertAt :: a -> [a] -> Int -> [a]
insertAt elt ls pos
    | pos > length ls = error "List index out of range."
    | pos == length ls = ls ++ [elt]
    | otherwise = [ls!!i | i <- [0..pos-1]] ++ [elt] ++ [ls!!i | i <- [pos..(length ls)-1]]
