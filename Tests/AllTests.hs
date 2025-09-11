module Main where

import System.Exit (exitFailure, exitSuccess)

-- IMPORTANTE: Adicione aqui os imports para TODOS os seus arquivos de teste
-- e as listas de testes que eles exportam.
import Tests.AlgorithmsTests (testesEspecialista4)
import Tests.CivilEngineeringTests (testesCivilEngineering)
import Tests.ElectricalEngineeringTests (testesEngenhariaEletrica)
import Tests.MechanicalEngineeringTests (testesEngenhariaMecanica)
import Tests.SortingTests (testesAlgoritmosOrdenacao)
import Tests.ValidationTests (testesEspecialista5)
import System.IO (hSetEncoding, stdout, utf8, BufferMode(NoBuffering), hSetBuffering)

main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetBuffering stdout NoBuffering
    putStrLn "\n--- EXECUTANDO SUÍTE DE TESTES COMPLETA ---\n"

    let allTests =
            testesEspecialista4
            ++ testesAlgoritmosOrdenacao
            ++ testesEngenhariaMecanica
            ++ testesEngenhariaEletrica
            ++ testesCivilEngineering
            ++ testesEspecialista5

    totalFailures <- runTests allTests

    if totalFailures == 0
        then exitSuccess
        else exitFailure

runTests :: [(String, Bool)] -> IO Int
runTests allTests = do
    failureCodes <- mapM runTest allTests
    let totalFailures = sum failureCodes
    let totalTests = length allTests
    let totalPassed = totalTests - totalFailures
    putStrLn "\n--------------------------------------"
    putStrLn "RESUMO DOS TESTES:"
    putStrLn $ "Total de Testes..: " ++ show totalTests
    putStrLn $ "Testes que Passaram: " ++ show totalPassed
    putStrLn $ "Testes que Falharam: " ++ show totalFailures
    putStrLn "--------------------------------------"
    if totalFailures == 0
        then putStrLn "Todos os testes passaram com sucesso!"
        else putStrLn "Alguns testes falharam."
    putStrLn ""
    return totalFailures

runTest :: (String, Bool) -> IO Int
runTest (nome, resultado) = do
    let status = if resultado then "[Passou]" else "[FALHOU]"
    let linha = status ++ " - " ++ nome
    putStrLn linha
    return (if resultado then 0 else 1)