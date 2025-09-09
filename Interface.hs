module Interface where

import Validation
import Calculus

prompt :: Read a => String -> IO a
prompt msg = do putStr msg
                readLn

exibirOpções :: [String] -> IO ()
exibirOpções opções = do
    putStrLn ""
    putStrLn "Opções:"
    putStrLn ""
    putStrLn (unlines opções)
    putStrLn ""

lerOpção :: Int -> Int -> IO Int
lerOpção min max = do
    putStr "Escolha uma opção: "
    opção <- readLn
    valida opção min max

menu :: [String] -> IO Int
menu texto = do exibirOpções texto
                lerOpção 0 (length texto - 1)

imprimirResultado :: a => String -> a -> IO ()
imprimirResultado msg res = do
    putStrLn ""
    putStrLn "----------------------------------------------------"
    putStrLn (msg ++ " " ++ show res)
    putStrLn "----------------------------------------------------"
    putStrLn ""

laçoMenuPrincipal :: IO ()
laçoMenuPrincipal = do
    opção <- menu ["1 - Módulos do Núcleo", "2 - Módulos de Engenharia", "0 - Sair"]
    case opção of
        1 -> laçoMenuEspecialistas
        2 -> laçoMenuEngenharias
        0 -> return ()

laçoMenuEspecialistas :: IO ()
laçoMenuEspecialistas = do
    opção <- menu [
                    "1 - Geometria Analítica e Computacional", 
                    "2 - Álgebra Linear e Operações com Matrizes", 
                    "3 - Cálculo Diferencial e Integral",
                    "4 - Algoritmos e Estruturas de Dados",
                    "5 - Validação, Relatórios e Interface",
                    "0 - Voltar a tela anterior"
                ]
    case opção of
        1 -> laçoMenuGA
        2 -> laçoMenuAL
        3 -> laçoMenuCalculo
        4 -> laçoMenuAED
        5 -> laçoMenuVal
        0 -> laçoMenuPrincipal

laçoMenuGA :: a => IO a
laçoMenuGA = do
    opção <- menu [
                    "1 - Distância entre pontos 2D", 
                    "2 - Distância entre pontos 3D", 
                    "3 - Ponto Médio",
                    "4 - Calcular Área",
                    "5 - Calcular Perimetro",
                    "6 - Calcular Volume",
                    "7 - Dentro do Poligono",
                    "8 - Interseção de Retas",
                    "0 - Voltar a tela anterior"
                ]
    case opção of
        1 -> 
        2 -> 
        3 -> 
        4 -> laçoMenuGAÁrea
        5 -> laçoMenuGAPerimetro
        6 -> laçoMenuGAVolume
        7 -> 
        8 -> 
        0 -> laçoMenuEspecialistas

laçoMenuGAÁrea :: a => IO a
laçoMenuGAÁrea = do
    opção <- menu [
                    "1 - Círculo", 
                    "2 - Triangulo", 
                    "3 - Poligono",
                    "4 - Esfera",
                    "5 - Cilíndro",
                    "6 - Paralelepipedo",
                    "0 - Voltar a tela anterior"
                ]
    case opção of
        1 -> 
        2 -> 
        3 -> 
        4 -> 
        5 -> 
        6 ->
        0 -> laçoMenuGA

laçoMenuGAPerimetro :: a => IO a
laçoMenuGAPerimetro = do
    opção <- menu [
                    "1 - Círculo", 
                    "2 - Triangulo", 
                    "3 - Poligono",
                    "4 - Esfera",
                    "5 - Cilíndro",
                    "6 - Paralelepipedo",
                    "0 - Voltar a tela anterior"
                ]
    case opção of
        1 -> 
        2 -> 
        3 -> 
        4 -> 
        5 -> 
        6 ->
        0 -> laçoMenuGA

laçoMenuGAVolume :: a => IO a
laçoMenuGAVolume = do
    opção <- menu [
                    "1 - Círculo", 
                    "2 - Triangulo", 
                    "3 - Poligono",
                    "4 - Esfera",
                    "5 - Cilíndro",
                    "6 - Paralelepipedo",
                    "0 - Voltar a tela anterior"
                ]
    case opção of
        1 -> 
        2 -> 
        3 -> 
        4 -> 
        5 -> 
        6 ->
        0 -> laçoMenuGA

laçoMenuAL :: a => IO a
laçoMenuAL = do
    opção <- menu [
                    "1 - Somar Matrizes", 
                    "2 - Multiplicar Matrizes", 
                    "3 - Transposta de Matriz",
                    "4 - Determinante",
                    "5 - Sistema Linear",
                    "6 - Produto Escalar",
                    "7 - Norma Vetor",
                    "8 - Ângulo Entre Vetores",
                    "0 - Voltar a tela anterior"
                ]
    case opção of
        1 -> 
        2 -> 
        3 -> 
        4 -> 
        5 -> 
        6 -> 
        7 -> 
        8 -> 
        0 -> laçoMenuEspecialistas

laçoMenuCalculo :: a => IO a
laçoMenuAL = do
    opção <- menu [
                    "1 - Avaliar Função", 
                    "2 - Derivada Numérica", 
                    "3 - Integral Numérica",
                    "4 - Encontrar Raízes",
                    "5 - Encontrar Máximo",
                    "6 - Encontrar Mínimo",
                    "7 - Comprimento de Curva",
                    "0 - Voltar a tela anterior"
                ]
    case opção of
        1 -> 
        2 -> 
        3 -> 
        4 -> 
        5 -> 
        6 -> 
        7 -> 
        0 -> laçoMenuEspecialistas

laçoMenuAED :: a -> IO a
laçoMenuAED = do
    opção <- menu [
                    "1 - Quick Sort",
                    "2 - Merge Sort",
                    "3 - Insertion Sort",
                    "4 - Buscar Projeto",
                    "5 - Inserir Ordenado",
                    "6 - Construir Árvore",
                    "7 - Buscar Árvore",
                    "8 - Filtrar Projetos",
                    "0 - Voltar a tela anterior"
                ]
    case opção of
        1 -> 
        2 -> 
        3 -> 
        4 -> 
        5 -> 
        6 -> 
        7 -> 
        8 -> 
        0 -> laçoMenuEspecialistas

laçoMenuVal :: a -> IO a
laçoMenuVal = do
    opção <- menu [
                    "1 - Validar Projeto",
                    "2 - Calcular Custo Total",
                    "3 - Gerar Relatório de Projeto",
                    "4 - Comparar Projetos",
                    "5 - Estátisticas Básicas",
                    "6 - Contar por Tipo",
                    "7 - Projeto em Atraso",
                    "0 - Voltar a tela anterior"
                ]
    case opção of
        1 -> 
        2 -> 
        3 -> 
        4 -> 
        5 -> 
        6 -> 
        7 -> 
        0 -> laçoMenuEspecialistas

laçoMenuEngenharias :: IO ()
laçoMenuEngenharias = do
    opção <- menu [
                    "1 - Engenharia Civil", 
                    "2 - Engenharia Mecânica", 
                    "3 - Engenharia Elétrica",
                    "0 - Voltar a tela anterior"
                ]
    case opção of
        1 -> laçoMenuCivil
        2 -> laçoMenuMecânica
        3 -> laçoMenuElétrica
        0 -> laçoMenuPrincipal

laçoMenuCivil :: a -> IO a
laçoMenuCivil = do
    opção <- menu [
                    "1 - Momento de Inércia Retângular",
                    "2 - Tensão Normal",
                    "3 - Deflexão de Viga",
                    "4 - Carga Crítica de Euler",
                    "5 - Volume de Concreto",
                    "0 - Voltar a tela anterior"
                ]
    case opção of
        1 -> 
        2 -> 
        3 -> 
        4 -> 
        5 -> 
        0 -> laçoMenuEngenharias

laçoMenuMecânica :: a -> IO a
laçoMenuMecânica = do
    opção <- menu [
                    "1 - Torque",
                    "2 - Velocidade Angular",
                    "3 - Aceleração Centripeta",
                    "4 - Energia Cinética",
                    "5 - Energia Potencial",
                    "6 - Centro de Massa",
                    "0 - Voltar a tela anterior"
                ]
    case opção of
        1 -> 
        2 -> 
        3 -> 
        4 -> 
        5 -> 
        6 -> 
        0 -> laçoMenuEngenharias

laçoMenuElétrica :: a -> IO a
laçoMenuElétrica = do
    opção <- menu [
                    "1 - Tensão",
                    "2 - Potência Elétrica (Tensão, Corrente)",
                    "3 - Potência Elétrica (Resistência, Corrente)",
                    "4 - Potência Elétrica (Tensão, Resistência)",
                    "5 - Resistência em Série",
                    "6 - Resistência em Paralelo",
                    "7 - Impedância AC",
                    "8 - Polar para Retângular",
                    "9 - Retângular para Polar",
                    "0 - Voltar a tela anterior"
                ]
    case opção of
        1 -> 
        2 -> 
        3 -> 
        4 -> 
        5 -> 
        6 -> 
        7 -> 
        8 -> 
        9 -> 
        0 -> laçoMenuEngenharias