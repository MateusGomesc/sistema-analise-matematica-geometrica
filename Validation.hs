module Validation where

import Types

valida :: Int -> Int -> Int -> IO Int
valida opção min max
    | min <= opção && opção <= max = return opção
    | otherwise = do 
        putStrLn ""
        putStrLn "Opção inválida!"
        putStrLn ""
        putStr "Escolha uma opção: "
        opção <- readLn
        valida opção min max