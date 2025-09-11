{-# LANGUAGE ScopedTypeVariables #-}

module Interface where

import Types
import Validation
import Calculus
import Algorithms
import LinearAlgebra
import Geometry
import DataStructures
import Reports
import Engineering.Civil
import Engineering.Electrical
import Engineering.Mechanical

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

imprimirResultado :: Show a => String -> a -> IO ()
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

laçoMenuGA :: IO ()
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
        1 -> do
            ponto1 <- prompt "Ponto 1: "
            ponto2 <- prompt "Ponto 2: "
            let resultado = distanciaEntrePontos ponto1 ponto2
            imprimirResultado "Resultado: " resultado
        2 -> do
            ponto1 <- prompt "Ponto 1: "
            ponto2 <- prompt "Ponto 2: "
            let resultado = distancia3D ponto1 ponto2
            imprimirResultado "Resultado: " resultado
        3 -> do
            ponto1 <- prompt "Ponto 1: "
            ponto2 <- prompt "Ponto 2: "
            let resultado = pontoMedio ponto1 ponto2
            imprimirResultado "Resultado: " resultado
        4 -> do
            figura <- prompt "Figura: "
            let resultado = calcularArea figura
            imprimirResultado "Resultado: " resultado
        5 -> do
            figura <- prompt "Figura: "
            let resultado = calcularPerimetro figura
            imprimirResultado "Resultado: " resultado
        6 -> do
            figura <- prompt "Figura: "
            let resultado = calcularVolume figura
            imprimirResultado "Resultado: " resultado
        7 -> do
            ponto <- prompt "Ponto: "
            pontos <- prompt "Pontos: "
            let resultado = dentroDoPoligono ponto pontos
            imprimirResultado "Resultado: " resultado
        8 -> do
            reta1 <- prompt "Reta 1: "
            reta2 <- prompt "Reta 2: "
            let resultado = intersecaoRetas reta1 reta2
            imprimirResultado "Resultado: " resultado
        0 -> laçoMenuEspecialistas

laçoMenuAL :: IO ()
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
        1 -> do
            matriz1 <- prompt "Matriz 1: "
            matriz2 <- prompt "Matriz 2: "
            let resultado = somarMatrizes matriz1 matriz2
            imprimirResultado "Resultado: " resultado
        2 -> do
            matriz1 <- prompt "Matriz 1: "
            matriz2 <- prompt "Matriz 2: "
            let resultado = multiplicarMatrizes matriz1 matriz2
            imprimirResultado "Resultado: " resultado
        3 -> do
            matriz <- prompt "Matriz: "
            let resultado = transpostaMatriz matriz
            imprimirResultado "Resultado: " resultado
        4 -> do
            matriz <- prompt "Matriz: "
            let resultado = determinante matriz
            imprimirResultado "Resultado: " resultado
        5 -> do
            matriz <- prompt "Matriz: "
            vetor <- prompt "Vetor: "
            let resultado = resolverSistemaLinear matriz vetor
            imprimirResultado "Resultado: " resultado
        6 -> do
            vetor1 <- prompt "Vetor: "
            vetor2 <- prompt "Vetor: "
            let resultado = produtoEscalar vetor1 vetor2
            imprimirResultado "Resultado: " resultado
        7 -> do
            vetor <- prompt "Vetor: "
            let resultado =normaVetor vetor
            imprimirResultado "Resultado: " resultado
        8 -> do
            vetor1 <- prompt "Vetor: "
            vetor2 <- prompt "Vetor: "
            let resultado = anguloEntreVetores vetor1 vetor2
            imprimirResultado "Resultado: " resultado
        0 -> laçoMenuEspecialistas

laçoMenuCalculo :: IO ()
laçoMenuCalculo = do
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
        1 -> do
            função <- prompt "Função: "
            x <- prompt "Valor de x: "
            let resultado = avaliarFuncao função x
            imprimirResultado "Resultado: " resultado
        2 -> do
            função <- prompt "Função: "
            x <- prompt "Ponto da derivada: "
            let resultado = derivadaNumerica função x
            imprimirResultado "Resultado: " resultado
        3 -> do
            função <- prompt "Função: "
            x1 <- prompt "Ínicio: "
            x2 <- prompt "Final: "
            let resultado = integralNumerica função x1 x2 1000
            imprimirResultado "Resultado: " resultado
        4 -> do
            função <- prompt "Função: "
            x1 <- prompt "Ínicio: "
            x2 <- prompt "Final: "
            let resultado = encontrarRaizes função x1 x2
            imprimirResultado "Resultado: " resultado
        5 -> do
            função <- prompt "Função: "
            x1 <- prompt "Ínicio: "
            x2 <- prompt "Final: "
            let resultado = encontrarMaximo função x1 x2
            imprimirResultado "Resultado: " resultado
        6 -> do
            função <- prompt "Função: "
            x1 <- prompt "Ínicio: "
            x2 <- prompt "Final: "
            let resultado = encontrarMinimo função x1 x2
            imprimirResultado "Resultado: " resultado
        7 -> do
            função <- prompt "Função: "
            x1 <- prompt "Ínicio: "
            x2 <- prompt "Final: "
            let resultado = calcularComprimentoCurva função x1 x2
            imprimirResultado "Resultado: " resultado
        0 -> laçoMenuEspecialistas
    laçoMenuCalculo

laçoMenuAED :: IO ()
laçoMenuAED = do
    opção <- menu [
                    "1 - Quick Sort",
                    "2 - Merge Sort",
                    "3 - Insertion Sort",
                    "4 - Buscar Projeto",
                    "5 - Inserir Ordenado",
                    "6 - Construir Árvore",
                    "7 - Buscar Árvore",
                    "8 - Filtrar Projetos de Engenharia Civil",
                    "0 - Voltar a tela anterior"
                ]
    case opção of
        1 -> do
            lista :: [Double] <- prompt "Lista: "
            let resultado = quickSort lista
            imprimirResultado "Resultado: " resultado
        2 -> do
            lista :: [Double] <- prompt "Lista: "
            let resultado = mergeSort lista
            imprimirResultado "Resultado: " resultado
        3 -> do
            lista :: [Double] <- prompt "Lista: "
            let resultado = insertionSort lista
            imprimirResultado "Resultado: " resultado
        4 -> do
            id <- prompt "Id do projeto: "
            listaDeProjetos :: [Projeto] <- prompt "Lista de projetos: "
            let resultado = buscarProjeto id listaDeProjetos
            imprimirResultado "Resultado: " resultado
        5 -> do
            elemento <- prompt "Elemento: "
            lista :: [Double] <- prompt "Lista: "
            let resultado = inserirOrdenado elemento lista
            imprimirResultado "Resultado: " resultado
        6 -> do
            vetor :: [Double] <- prompt "Vetor: "
            let resultado = construirArvore vetor
            imprimirResultado "Resultado: " resultado
        7 -> do
            elemento :: Double <- prompt "Elemento: "
            árvore :: ArvoreBinaria Double <- prompt "Árvore: "
            let resultado = buscarArvore elemento árvore
            imprimirResultado "Resultado: " resultado
        8 -> do
            listaDeProjetos :: [Projeto] <- prompt "Lista de projetos: "
            let resultado = filtrarProjetos (\p -> tipoProjeto p == Civil) listaDeProjetos
            imprimirResultado "Resultado: " resultado
        0 -> laçoMenuEspecialistas

laçoMenuVal :: IO ()
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
        1 -> do
            projeto <- prompt "Projeto: "
            mapM_ putStrLn (validarProjeto projeto)
        2 -> return ()
        3 -> do
            projeto <- prompt "Projeto: "
            mapM_ putStrLn (gerarRelatorioProjeto projeto)
        4 -> do
            projeto1 <- prompt "Projeto 1: "
            projeto2 <- prompt "Projeto 2: "
            putStrLn (compararProjetos projeto1 projeto2)
        5 -> return ()
        6 -> do
            projetos :: [Projeto] <- prompt "Lista de projetos: "
            let resultado = contarPorTipo projetos
            imprimirResultado "Resultado: " resultado
        7 -> do
            projetos :: [Projeto] <- prompt "Lista de projetos: "
            dataLimite <- prompt "Data limite: "
            let resultado = projetosEmAtraso projetos dataLimite
            imprimirResultado "Resultado: " resultado
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

laçoMenuCivil :: IO ()
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
        1 -> do
            largura <- prompt "Largura: "
            altura <- prompt "Altura: "
            let resultado = momentoInerciaRetangular largura altura
            imprimirResultado "Resultado: " resultado
        2 -> do
            força <- prompt "Força: "
            área <- prompt "Área: "
            let resultado = tensaoNormal força área
            imprimirResultado "Resultado: " resultado
        3 -> do
            força <- prompt "Força: "
            comprimento <- prompt "Comprimento: "
            moduloElasticidade <- prompt "Modulo de elasticidade: "
            momentoInercia <- prompt "Momento de Ínercia: "
            let resultado = deflexaoViga força comprimento moduloElasticidade momentoInercia
            imprimirResultado "Resultado: " resultado
        4 -> do
            moduloElasticidade <- prompt "Modulo de elasticidade: "
            momentoInercia <- prompt "Momento de Ínercia: "
            comprimento <- prompt "Comprimento: "
            let resultado = cargaCriticaEuler moduloElasticidade momentoInercia comprimento
            imprimirResultado "Resultado: " resultado
        5 -> do
            figura <- prompt "Figura: "
            let resultado = volumeConcreto figura
            imprimirResultado "Resultado: " resultado
        0 -> laçoMenuEngenharias

laçoMenuMecânica :: IO ()
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
        1 -> do
            força <- prompt "Força: "
            distância <- prompt "Distância: "
            ângulo <- prompt "Ângulo: "
            let resultado = calcularTorque força distância ângulo
            imprimirResultado "Resultado: " resultado
        2 -> do
            velocidade <- prompt "Velocidade: "
            raio <- prompt "Raio: "
            let resultado = velocidadeAngular velocidade raio
            imprimirResultado "Resultado: " resultado
        3 -> do
            velocidade <- prompt "Velocidade: "
            raio <- prompt "Raio: "
            let resultado = aceleracaoCentripeta velocidade raio
            imprimirResultado "Resultado: " resultado
        4 -> do
            massa <- prompt "Massa: "
            velocidade <- prompt "Velocidade: "
            let resultado = energiaCinetica massa velocidade
            imprimirResultado "Resultado: " resultado
        5 -> do
            massa <- prompt "Massa: "
            altura <- prompt "Altura: "
            let resultado = energiaCinetica massa altura
            imprimirResultado "Resultado: " resultado
        6 -> do
            massa :: [(Massa, Distancia)] <- prompt "Massa: "
            let resultado = centroMassaX massa
            imprimirResultado "Resultado: " resultado
        0 -> laçoMenuEngenharias

laçoMenuElétrica :: IO ()
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
        1 -> do
            resistencia <- prompt "Resitência: "
            corrente <- prompt "Corrente: "
            let resultado = tensaoOhm resistencia corrente
            imprimirResultado "Resultado: " resultado
        2 -> return ()
        3 -> do
            resistencia <- prompt "Resitência: "
            corrente <- prompt "Corrente: "
            let resultado = potenciaEletricaRI resistencia corrente
            imprimirResultado "Resultado: " resultado
        4 -> do
            tensão <- prompt "Tensão: "
            resistencia <- prompt "Resitência: "
            let resultado = potenciaEletricaVR tensão resistencia
            imprimirResultado "Resultado: " resultado
        5 -> return ()
        6 -> do
            listaResistências <- prompt "Lista de Resistência: "
            let resultado = resistenciaParalelo listaResistências
            imprimirResultado "Resultado: " resultado
        7 -> do
            resistência <- prompt "Resistência: "
            reatencia <- prompt "Reatencia: "
            let resultado = impedanciaAC resistência reatencia
            imprimirResultado "Resultado: " resultado
        8 -> do
            raio <- prompt "Raio: "
            theta <- prompt "Theta: "
            let resultado = polarParaRetangular raio theta
            imprimirResultado "Resultado: " resultado
        9 -> do
            x <- prompt "X: "
            y <- prompt "Y: "
            let resultado = retangularParaPolar x y
            imprimirResultado "Resultado: " resultado
        0 -> laçoMenuEngenharias