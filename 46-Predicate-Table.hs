and' :: Bool -> Bool -> Bool
and' a b = if a then b else a

-- not actually required, but i wanted to do all the logic myself
not' :: Bool -> Bool
not' a = if a then False else True

or' :: Bool -> Bool -> Bool
or' a b
    | a = a
    | b = b
    | otherwise = a

nand' :: Bool -> Bool -> Bool
nand' a b = not' (and' a b)

nor' :: Bool -> Bool -> Bool
nor' a b = not' (or' a b)

xor' :: Bool -> Bool -> Bool
xor' a b = if a then not' b else b

impl' :: Bool -> Bool -> Bool
impl' p q = or' (not' p) (q)

equ' :: Bool -> Bool -> Bool
equ' a b = not' (xor' a b)

table :: (Bool -> Bool -> Bool) -> IO()
table fun = putStrLn $ concat $ map (++ "\n")
            [show a ++ " " ++ show b ++ " " ++ show (fun a b)
            | a <- [True, False], b <- [True, False]]
            
