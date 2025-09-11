module Reports where

import Types
import Data.Time.Calendar
import Text.Printf (printf)



formatarMaterial :: Material -> String
formatarMaterial mat =
  "  - " ++ nomeMaterial mat
    ++ " (Custo Unitário: R$ "
    ++ show (custo mat)
    ++ ", Quantidade: "
    ++ show (quantidade mat)
    ++ ")"

formatarCalculo :: CalculoMat -> String
formatarCalculo calc =
  "  - " ++ nomeCalculo calc
    ++ ": "
    ++ formula calc
    ++ " = "
    ++ show (resultado calc)
    ++ " "
    ++ unidade calc

formatarFuncao :: Funcao -> String
formatarFuncao (Funcao _ descricao) = "  - " ++ descricao

gerarRelatorioProjeto :: Projeto -> [String]
gerarRelatorioProjeto p =
  let
    cabeçalho =
      [ "==============================================",
        "      RELATÓRIO DE PROJETO",
        "==============================================",
        ""
      ]
    infoBasica =
      [ "ID do Projeto: " ++ show (idProjeto p),
        "Nome:          " ++ nomeProjeto p,
        "Tipo:          " ++ show (tipoProjeto p),
        "Status:        " ++ show (statusProjeto p),
        "Orçamento:     R$ " ++ printf "%.2f" (orcamento p),
        ""
      ]
    datas =
      [ "Data de Início: " ++ show (dataInicio p),
        "Data de Fim:    " ++ show (dataFim p),
        ""
      ]
    secaoMateriais =
      if null (materiais p)
        then ["Seção de Materiais: (vazia)"]
        else ["Seção de Materiais:"] ++ map formatarMaterial (materiais p)
    secaoCalculos =
      if null (calculos p)
        then ["", "Seção de Cálculos: (vazia)"]
        else ["", "Seção de Cálculos:"] ++ map formatarCalculo (calculos p)
    secaoFuncoes =
      if null (funcoes p)
        then ["", "Seção de Funções Matemáticas: (vazia)"]
        else ["", "Seção de Funções Matemáticas:"] ++ map formatarFuncao (funcoes p)
    rodape = ["", "=============================================="]
   in
    concat [cabeçalho, infoBasica, datas, secaoMateriais, secaoCalculos, secaoFuncoes, rodape]

showMaybeData :: Maybe Day -> String
showMaybeData Nothing  = "Não definida"
showMaybeData (Just d) = show d

compararMateriais :: String -> String -> String
compararMateriais m1 m2 =
  "Materiais : " ++ m1 ++ " vs " ++ m2 ++
    if m1 == m2 then " (Iguais)" else " (Diferentes)"

compararComplexidade :: String -> String -> String
compararComplexidade c1 c2 =
  "Complexidade : " ++ c1 ++ " vs " ++ c2 ++
    if c1 == c2 then " (Iguais)" else " (Diferentes)"

compararOrcamento :: Float -> Float -> String
compararOrcamento o1 o2 =
  let s1 = "R$ " ++ show o1
      s2 = "R$ " ++ show o2
      comparacao
        | abs (o1 - o2) < 0.01 = " (Iguais)"
        | o1 > o2  = " (Maior que o outro)"
        | otherwise = " (Menor que o outro)"
  in "Orçamento : " ++ s1 ++ " vs " ++ s2 ++ comparacao

compararCustoTotal :: Float -> Float -> String
compararCustoTotal c1 c2 =
  let s1 = "R$ " ++ show c1
      s2 = "R$ " ++ show c2
      comparacao
        | abs (c1 - c2) < 0.01 = " (Iguais)"
        | c1 > c2  = " (Maior que o outro)"
        | otherwise = " (Menor que o outro)"
  in "Custo Total : " ++ s1 ++ " vs " ++ s2 ++ comparacao

calcularCustoTotal :: Projeto -> Custo
calcularCustoTotal projeto =
  sum [preco * quantidade | Material _ _ _ preco quantidade <- materiais projeto]

compararLinha :: String -> String -> String -> String
compararLinha atributo val1 val2
  | atributo == "Materiais"    = compararMateriais val1 val2
  | atributo == "Complexidade" = compararComplexidade val1 val2
  | atributo == "Orçamento"    =
      compararOrcamento (read val1 :: Float) (read val2 :: Float)
  | atributo == "Custo Total"  =
      compararCustoTotal (read val1 :: Float) (read val2 :: Float)
  | otherwise = atributo ++ " : " ++ val1 ++ " vs " ++ val2 ++
      if val1 == val2 then " (Iguais)" else " (Diferentes)"

compararProjetos :: Projeto -> Projeto -> String
compararProjetos p1 p2
  | p1 == p2 = "São iguais"
  | otherwise =
      unlines
        [ "=========================================="
        , "      COMPARAÇÃO DE PROJETOS"
        , "=========================================="
        , compararLinha "ID" (show $ idProjeto p1) (show $ idProjeto p2)
        , compararLinha "Nome" (nomeProjeto p1) (nomeProjeto p2)
        , compararLinha "Tipo" (show $ tipoProjeto p1) (show $ tipoProjeto p2)
        , compararLinha "Status" (show $ statusProjeto p1) (show $ statusProjeto p2)
        , compararLinha "Orçamento" (printf "%.2f" $ orcamento p1) (printf "%.2f" $ orcamento p2)
        , compararLinha "Data de Início" (show $ dataInicio p1) (show $ dataInicio p2)
        , compararLinha "Data de Fim" (showMaybeData $ dataFim p1) (showMaybeData $ dataFim p2)
        , ""
        , "-- Detalhes de Complexidade --"
        , compararLinha "Nº de Materiais" (show $ length $ materiais p1) (show $ length $ materiais p2)
        , compararLinha "Nº de Cálculos" (show $ length $ calculos p1) (show $ length $ calculos p2)
        , compararLinha "Nº de Funções" (show $ length $ funcoes p1) (show $ length $ funcoes p2)
        , "=========================================="
        ]
      
estatisticasBasicas :: [Double] -> (Double, Double, Double)
estatisticasBasicas [] = (0, 0, 0)
estatisticasBasicas xs = (media, maximo, minimo)
  where
    media = sum xs / fromIntegral (length xs)
    maximo = maximum xs
    minimo = minimum xs

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
        Just fim -> fim < dataAtual && statusProjeto projeto /= Concluido
        Nothing  -> False
