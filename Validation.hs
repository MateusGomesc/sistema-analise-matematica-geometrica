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

contarPorTipo :: [Projeto] -> [(TipoProjeto, Int)]
contarPorTipo projetos =
  filter (\(_, n) -> n > 0)  -- Filtra n > 0. So lista se tiver pelo menos um projeto daquele tipo
    [ (Civil,      length (filter (\p -> tipoProjeto p == Civil) projetos))
    , (Mecanica,   length (filter (\p -> tipoProjeto p == Mecanica) projetos))
    , (Eletrica,   length (filter (\p -> tipoProjeto p == Eletrica) projetos))
    , (Estrutural, length (filter (\p -> tipoProjeto p == Estrutural) projetos))
    ]

projetosEmAtraso :: [Projeto] -> Day -> [Projeto]
projetosEmAtraso projetos dataAtual =
  filter emAtraso projetos
  where
    emAtraso projeto =
      case dataFim projeto of
        Just fim -> fim < dataAtual && status projeto /= Concluido
        Nothing  -> False
