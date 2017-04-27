-- Warning: 1-indexed (not by choice)
kthElt :: (Ord a, Num b, Ord b) => [a] -> b -> a
kthElt [] n = error "Idx out of bounds"
kthElt [x] n = if (n==1) then x else error "Idx out of bounds"
kthElt (x:xs) n = if (n==1) then x else kthElt xs (n-1)
