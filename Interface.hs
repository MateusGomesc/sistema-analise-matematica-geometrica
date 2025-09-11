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
lerOpção minimo maximo = do
    putStr "Escolha uma opção: "
    opção <- readLn
    valida opção minimo maximo

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

lacoMenuPrincipal :: IO ()
lacoMenuPrincipal = do
    opção <- menu ["1 - Módulos do Núcleo", "2 - Módulos de Engenharia", "0 - Sair"]
    case opção of
        1 -> laçoMenuEspecialistas
        2 -> laçoMenuEngenharias
        0 -> return ()
        _ -> do
            putStrLn "Opção Inválida!"
            lacoMenuPrincipal

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
        0 -> lacoMenuPrincipal
        _ -> do
            putStrLn "Opção Inválida!"
            laçoMenuEspecialistas

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
            let res = distanciaEntrePontos ponto1 ponto2
            imprimirResultado "Resultado: " res
        2 -> do
            ponto1 <- prompt "Ponto 1: "
            ponto2 <- prompt "Ponto 2: "
            let res = distancia3D ponto1 ponto2
            imprimirResultado "Resultado: " res
        3 -> do
            ponto1 <- prompt "Ponto 1: "
            ponto2 <- prompt "Ponto 2: "
            let res = pontoMedio ponto1 ponto2
            imprimirResultado "Resultado: " res
        4 -> do
            figura <- prompt "Figura: "
            let res = calcularArea figura
            imprimirResultado "Resultado: " res
        5 -> do
            figura <- prompt "Figura: "
            let res = calcularPerimetro figura
            imprimirResultado "Resultado: " res
        6 -> do
            figura <- prompt "Figura: "
            let res = calcularVolume figura
            imprimirResultado "Resultado: " res
        7 -> do
            ponto <- prompt "Ponto: "
            pontos <- prompt "Pontos: "
            let res = dentroDoPoligono ponto pontos
            imprimirResultado "Resultado: " res
        8 -> do
            reta1 <- prompt "Reta 1: "
            reta2 <- prompt "Reta 2: "
            let res = intersecaoRetas reta1 reta2
            imprimirResultado "Resultado: " res
        0 -> laçoMenuEspecialistas
        _ -> do
            putStrLn "Opção Inválida!"
            laçoMenuGA

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
            let res = somarMatrizes matriz1 matriz2
            imprimirResultado "Resultado: " res
        2 -> do
            matriz1 <- prompt "Matriz 1: "
            matriz2 <- prompt "Matriz 2: "
            let res = multiplicarMatrizes matriz1 matriz2
            imprimirResultado "Resultado: " res
        3 -> do
            matriz <- prompt "Matriz: "
            let res = transpostaMatriz matriz
            imprimirResultado "Resultado: " res
        4 -> do
            matriz <- prompt "Matriz: "
            let res = determinante matriz
            imprimirResultado "Resultado: " res
        5 -> do
            matriz <- prompt "Matriz: "
            vetor <- prompt "Vetor: "
            let res = resolverSistemaLinear matriz vetor
            imprimirResultado "Resultado: " res
        6 -> do
            vetor1 <- prompt "Vetor: "
            vetor2 <- prompt "Vetor: "
            let res = produtoEscalar vetor1 vetor2
            imprimirResultado "Resultado: " res
        7 -> do
            vetor <- prompt "Vetor: "
            let res =normaVetor vetor
            imprimirResultado "Resultado: " res
        8 -> do
            vetor1 <- prompt "Vetor: "
            vetor2 <- prompt "Vetor: "
            let res = anguloEntreVetores vetor1 vetor2
            imprimirResultado "Resultado: " res
        0 -> laçoMenuEspecialistas
        _ -> do
            putStrLn "Opção Inválida!"
            laçoMenuAL

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
            let res = avaliarFuncao função x
            imprimirResultado "Resultado: " res
        2 -> do
            função <- prompt "Função: "
            x <- prompt "Ponto da derivada: "
            let res = derivadaNumerica função x
            imprimirResultado "Resultado: " res
        3 -> do
            função <- prompt "Função: "
            x1 <- prompt "Ínicio: "
            x2 <- prompt "Final: "
            let res = integralNumerica função x1 x2 1000
            imprimirResultado "Resultado: " res
        4 -> do
            função <- prompt "Função: "
            x1 <- prompt "Ínicio: "
            x2 <- prompt "Final: "
            let res = encontrarRaizes função x1 x2
            imprimirResultado "Resultado: " res
        5 -> do
            função <- prompt "Função: "
            x1 <- prompt "Ínicio: "
            x2 <- prompt "Final: "
            let res = encontrarMaximo função x1 x2
            imprimirResultado "Resultado: " res
        6 -> do
            função <- prompt "Função: "
            x1 <- prompt "Ínicio: "
            x2 <- prompt "Final: "
            let res = encontrarMinimo função x1 x2
            imprimirResultado "Resultado: " res
        7 -> do
            função <- prompt "Função: "
            x1 <- prompt "Ínicio: "
            x2 <- prompt "Final: "
            let res = calcularComprimentoCurva função x1 x2
            imprimirResultado "Resultado: " res
        0 -> laçoMenuEspecialistas
        _ -> do
            putStrLn "Opção Inválida!"
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
            let res = quickSort lista
            imprimirResultado "Resultado: " res
        2 -> do
            lista :: [Double] <- prompt "Lista: "
            let res = mergeSort lista
            imprimirResultado "Resultado: " res
        3 -> do
            lista :: [Double] <- prompt "Lista: "
            let res = insertionSort lista
            imprimirResultado "Resultado: " res
        4 -> do
            idProj <- prompt "Id do projeto: "
            listaDeProjetos :: [Projeto] <- prompt "Lista de projetos: "
            let res = buscarProjeto idProj listaDeProjetos
            imprimirResultado "Resultado: " res
        5 -> do
            elemento <- prompt "Elemento: "
            lista :: [Double] <- prompt "Lista: "
            let res = inserirOrdenado elemento lista
            imprimirResultado "Resultado: " res
        6 -> do
            vetor :: [Double] <- prompt "Vetor: "
            let res = construirArvore vetor
            imprimirResultado "Resultado: " res
        7 -> do
            elemento :: Double <- prompt "Elemento: "
            árvore :: ArvoreBinaria Double <- prompt "Árvore: "
            let res = buscarArvore elemento árvore
            imprimirResultado "Resultado: " res
        8 -> do
            listaDeProjetos :: [Projeto] <- prompt "Lista de projetos: "
            let res = filtrarProjetos (\p -> tipoProjeto p == Civil) listaDeProjetos
            imprimirResultado "Resultado: " res
        0 -> laçoMenuEspecialistas
        _ -> do
            putStrLn "Opção Inválida!"
            laçoMenuAED

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
        5 -> do
            valores :: [Double] <- prompt "Valores: "
            let res = estatisticasBasicas valores
            imprimirResultado "Resultado: " res
        6 -> do
            projetos :: [Projeto] <- prompt "Lista de projetos: "
            let res = contarPorTipo projetos
            imprimirResultado "Resultado: " res
        7 -> do
            projetos :: [Projeto] <- prompt "Lista de projetos: "
            dataLimite <- prompt "Data limite: "
            let res = projetosEmAtraso projetos dataLimite
            imprimirResultado "Resultado: " res
        0 -> laçoMenuEspecialistas
        _ -> do
            putStrLn "Opção Inválida!"
            laçoMenuVal

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
        0 -> lacoMenuPrincipal
        _ -> do
            putStrLn "Opção Inválida!"
            laçoMenuEngenharias

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
            let res = momentoInerciaRetangular largura altura
            imprimirResultado "Resultado: " res
        2 -> do
            força <- prompt "Força: "
            área <- prompt "Área: "
            let res = tensaoNormal força área
            imprimirResultado "Resultado: " res
        3 -> do
            força <- prompt "Força: "
            comprimento <- prompt "Comprimento: "
            moduloElasticidade <- prompt "Modulo de elasticidade: "
            momentoInercia <- prompt "Momento de Ínercia: "
            let res = deflexaoViga força comprimento moduloElasticidade momentoInercia
            imprimirResultado "Resultado: " res
        4 -> do
            moduloElasticidade <- prompt "Modulo de elasticidade: "
            momentoInercia <- prompt "Momento de Ínercia: "
            comprimento <- prompt "Comprimento: "
            let res = cargaCriticaEuler moduloElasticidade momentoInercia comprimento
            imprimirResultado "Resultado: " res
        5 -> do
            figura <- prompt "Figura: "
            let res = volumeConcreto figura
            imprimirResultado "Resultado: " res
        0 -> laçoMenuEngenharias
        _ -> do
            putStrLn "Opção Inválida!"
            laçoMenuCivil

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
            let res = calcularTorque força distância ângulo
            imprimirResultado "Resultado: " res
        2 -> do
            velocidade <- prompt "Velocidade: "
            raio <- prompt "Raio: "
            let res = velocidadeAngular velocidade raio
            imprimirResultado "Resultado: " res
        3 -> do
            velocidade <- prompt "Velocidade: "
            raio <- prompt "Raio: "
            let res = aceleracaoCentripeta velocidade raio
            imprimirResultado "Resultado: " res
        4 -> do
            massa <- prompt "Massa: "
            velocidade <- prompt "Velocidade: "
            let res = energiaCinetica massa velocidade
            imprimirResultado "Resultado: " res
        5 -> do
            massa <- prompt "Massa: "
            altura <- prompt "Altura: "
            let res = energiaCinetica massa altura
            imprimirResultado "Resultado: " res
        6 -> do
            massa :: [(Massa, Distancia)] <- prompt "Massa: "
            let res = centroMassaX massa
            imprimirResultado "Resultado: " res
        0 -> laçoMenuEngenharias
        _ -> do
            putStrLn "Opção Inválida!"
            laçoMenuMecânica

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
            resist <- prompt "Resitência: "
            corrente <- prompt "Corrente: "
            let res = tensaoOhm resist corrente
            imprimirResultado "Resultado: " res
        2 -> return ()
        3 -> do
            resist <- prompt "Resitência: "
            corrente <- prompt "Corrente: "
            let res = potenciaEletricaRI resist corrente
            imprimirResultado "Resultado: " res
        4 -> do
            tensão <- prompt "Tensão: "
            resist <- prompt "Resitência: "
            let res = potenciaEletricaVR tensão resist
            imprimirResultado "Resultado: " res
        5 -> return ()
        6 -> do
            listaResistências <- prompt "Lista de Resistência: "
            let res = resistenciaParalelo listaResistências
            imprimirResultado "Resultado: " res
        7 -> do
            resistência <- prompt "Resistência: "
            reatencia <- prompt "Reatencia: "
            let res = impedanciaAC resistência reatencia
            imprimirResultado "Resultado: " res
        8 -> do
            raio <- prompt "Raio: "
            theta <- prompt "Theta: "
            let res = polarParaRetangular raio theta
            imprimirResultado "Resultado: " res
        9 -> do
            x <- prompt "X: "
            y <- prompt "Y: "
            let res = retangularParaPolar x y
            imprimirResultado "Resultado: " res
        0 -> laçoMenuEngenharias
        _ -> do
            putStrLn "Opção Inválida!"
            laçoMenuElétrica