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

validarProjeto :: Projeto -> [String]
validarProjeto p =
  concat
    [ if idProjeto p <= 0 then ["O id do projeto deve ser positivo."] else []
    , if null (nomeProjeto p) then ["O nome do projeto não pode ser vazio."] else []
    , if length (nomeProjeto p) > 200 then ["O nome do projeto é muito longo."] else []
    , if orcamento p <= 0 then ["O orçamento deve ser positivo."] else []
    , if null (materiais p) then ["O projeto deve conter pelo menos um material."] else []
    , concatMap validarMaterial (materiais p)
    , case dataFim p of
        Just fim | fim < dataInicio p -> ["A data final não pode ser anterior à data inicial."]
        _ -> []
    ]
  where
    validarMaterial m =
      [ msg
      | (cond, msg) <-
          [ (densidade m <= 0, "A densidade do material " ++ nome m ++ " deve ser positiva.")
          , (resistencia m <= 0, "A resistência do material " ++ nome m ++ " deve ser positiva.")
          , (custo m <= 0, "O custo do material " ++ nome m ++ " deve ser positivo.")
          , (quantidade m < 0, "A quantidade do material " ++ nome m ++ " não pode ser negativa.")
          ]
      , cond
      ]
