module Algorithms where

import Types

quickSort :: (Ord a) => [a] -> [a]
quickSort []     = []
quickSort (x:xs) =
    let menores = [a | a <- xs, a <= x]
        maiores = [a | a <- xs, a >  x]
    in  quickSort menores ++ [x] ++ quickSort maiores
    
mergeSort :: (Ord a) => [a] -> [a]
mergeSort []  = []
mergeSort [x] = [x]
mergeSort xs  = merge (mergeSort left) (mergeSort right)
  where
    mid = length xs `div` 2
    (left, right) = splitAt mid xs

    merge [] ys = ys
    merge xs [] = xs
    merge (x:xs) (y:ys)
      | x <= y    = x : merge xs (y:ys)
      | otherwise = y : merge (x:xs) ys
      
insertionSort :: (Ord a) => [a] -> [a]
insertionSort []     = []
insertionSort (x:xs) = insert x (insertionSort xs)
  where
    insert :: (Ord a) => a -> [a] -> [a]
    insert y [] = [y]
    insert y (z:zs)
      | y <= z    = y : z : zs
      | otherwise = z : insert y zs

buscarProjeto :: Int -> [Projeto] -> Maybe Projeto
buscarProjeto _ [] = Nothing
buscarProjeto idBuscado (p:ps)
  | idProjeto p == idBuscado = Just p
  | otherwise                 = buscarProjeto idBuscado ps
      
inserirOrdenado :: (Ord a) => a -> [a] -> [a]
inserirOrdenado x [] = [x] 
inserirOrdenado x (y:ys)
  | x <= y    = x : y : ys
  | otherwise = y : inserirOrdenado x ys 

