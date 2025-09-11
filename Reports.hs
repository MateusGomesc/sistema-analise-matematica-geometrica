module Reports where

import Types
import Data.Time.Calendar

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
        "Status:        " ++ show (status p),
        "Orçamento:     R$ " ++ show (orcamento p),
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

compararLinha :: String -> String -> String -> String
compararLinha atributo val1 val2 =
  let
    padding = 20
    attrPad = atributo ++ replicate (padding - length atributo) ' '
    indicador = if val1 == val2 then " (Iguais)" else " (Diferentes)"
   in
    attrPad ++ ": " ++ val1 ++ " vs " ++ val2 ++ indicador

showMaybeData :: Maybe Day -> String
showMaybeData Nothing = "Não definida"
showMaybeData (Just d) = show d

compararProjetos :: Projeto -> Projeto -> String
compararProjetos p1 p2
  | p1 == p2 = "Os projetos são idênticos em todos os campos."
  | otherwise =
      unlines $
        [ "==========================================",
          "      COMPARAÇÃO DE PROJETOS",
          "==========================================",
          compararLinha "ID" (show $ idProjeto p1) (show $ idProjeto p2),
          compararLinha "Nome" (nomeProjeto p1) (nomeProjeto p2),
          compararLinha "Tipo" (show $ tipoProjeto p1) (show $ tipoProjeto p2),
          compararLinha "Status" (show $ status p1) (show $ status p2),
          compararLinha "Orçamento" ("R$ " ++ show (orcamento p1)) ("R$ " ++ show (orcamento p2)),
          compararLinha "Data de Início" (show $ dataInicio p1) (show $ dataInicio p2),
          compararLinha "Data de Fim" (showMaybeData $ dataFim p1) (showMaybeData $ dataFim p2),
          "",
          "-- Detalhes de Complexidade --",
          compararLinha "Nº de Materiais" (show $ length $ materiais p1) (show $ length $ materiais p2),
          compararLinha "Nº de Cálculos" (show $ length $ calculos p1) (show $ length $ calculos p2),
          compararLinha "Nº de Funções" (show $ length $ funcoes p1) (show $ length $ funcoes p2),
          "=========================================="
        ]

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
