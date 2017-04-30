data Code a = Single a | Mult Int a deriving Show

-- N.B.: Only decodes encode' type encodings (i.e. from #11), NOT original encode (#10).
decode :: Eq a => [Code a] -> [a]
decode [] = []
decode ((Single x):xs) = [x] ++ decode xs
decode ((Mult n x):xs) = replicate n x ++ decode xs
