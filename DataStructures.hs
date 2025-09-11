module DataStructures where

import Types

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
