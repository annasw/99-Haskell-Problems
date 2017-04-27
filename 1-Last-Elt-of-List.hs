lastElt :: [a] -> a
lastElt [] = error "There's no last elt in an empty list."
lastElt [x] = x
lastElt (x:xs) = lastElt xs
