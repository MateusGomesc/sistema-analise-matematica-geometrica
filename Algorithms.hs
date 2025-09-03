import Types

quickSort :: (Ord a) => [a] -> [a]
quickSort []     = []
quickSort (x:xs) =
    let menores = [a | a <- xs, a <= x]
        maiores = [a | a <- xs, a >  x]
    in  quickSort menores ++ [x] ++ quickSort maiores
    
