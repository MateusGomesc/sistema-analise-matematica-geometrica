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

  
inserir :: (Ord a) => a -> ArvoreBinaria a -> ArvoreBinaria a
inserir x Vazia = No x Vazia Vazia
inserir x (No y esq dir)
  | x <= y    = No y (inserir x esq) dir
  | otherwise = No y esq (inserir x dir)

construirArvore :: (Ord a) => [a] -> ArvoreBinaria a
construirArvore = foldr inserir Vazia


buscarArvore :: (Ord a) => a -> ArvoreBinaria a -> Bool
buscarArvore _ Vazia = False
buscarArvore x (No y esq dir)
  | x == y    = True
  | x < y     = buscarArvore x esq
  | otherwise = buscarArvore x dir
  

filtrarProjetos :: (Projeto -> Bool) -> [Projeto] -> [Projeto]
filtrarProjetos _ [] = []
filtrarProjetos f (p:ps)
  | f p       = p : filtrarProjetos f ps
  | otherwise = filtrarProjetos f ps

