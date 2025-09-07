module Calculus where

import Types

avaliarFuncao :: Funcao -> Double -> Double
avaliarFuncao (Funcao (Linear a b) _) x = a * x + b
avaliarFuncao (Funcao (Quadratica a b c) _) x = a * x^2 + b * x + c
avaliarFuncao (Funcao (Exponencial a b) _) x = a * exp (b*x)
avaliarFuncao (Funcao (Logaritmica a b) _) x = a * log (b*x)
avaliarFuncao (Funcao (Trigonometrica Seno a b) _) x = if b /= 0 then a * sin (b * x) else a * sin x
avaliarFuncao (Funcao (Trigonometrica Cosseno a b) _) x = a * cos (b * x)
avaliarFuncao (Funcao (Trigonometrica Tangente a b) _) x = a * tan (b * x)

derivadaNumerica :: Funcao -> Double -> Double
derivadaNumerica (Funcao (Linear a b) _) x = if a /= 0 then a else 0
derivadaNumerica (Funcao (Quadratica a 0 0) _) x = 2 * a * x
derivadaNumerica (Funcao (Trigonometrica Seno a 0) _) x = a * cos x
derivadaNumerica (Funcao (Trigonometrica Cosseno a 0) _) x = a * (-sin x)
derivadaNumerica (Funcao (Trigonometrica Tangente a 0) _) x = a * (1 / cos x)^2

integralNumerica :: Funcao -> Double -> Double -> Int -> Double
integralNumerica (Funcao (Linear a 0) _) x1 x2 _ = a * ((x2^2) / 2) - a * ((x1^2) / 2)
integralNumerica (Funcao (Linear 0 b) _) x1 x2 _ = b * x2 - b * x1
integralNumerica (Funcao (Quadratica a 0 0) _) x1 x2 _ = a * ((x2^3) / 3) - a * ((x1^3) / 3)
integralNumerica (Funcao (Trigonometrica Seno a 0) _) x1 x2 _ = - cos x2 + cos x1

encontrarRaizes :: Funcao -> Double -> Double -> [Double]
encontrarRaizes (Funcao (Linear 1 b) _) x1 x2 = let res = -b in if x1 <= res && res <= x2 then [res] else []
encontrarRaizes (Funcao (Linear 0 _) _) _ _ = []
encontrarRaizes (Funcao (Quadratica a 0 c) _) x1 x2 = let res = sqrt ((-c) / a) in if x1 <= res && x1 <= -x1 && x2 >= res && x2 >= -res then [res, -res] else []
encontrarRaizes (Funcao (Quadratica a b c) _) x1 x2 = let res = -(b / 2) in if x1 <= res && res <= x2 then [res] else []

encontrarMaximo :: Funcao -> Double -> Double -> Maybe Double
encontrarMaximo funcao x1 x2 = let 
                                passos = 10000
                                passo = (x2 - x1) / passos
                                valoresX = [x1 + passo * i | i <- [0..passos]]
                                valoresY = [(avaliarFuncao funcao x) | x <- valoresX]
                            in if null valoresY then Nothing else Just (maximum valoresY)

encontrarMinimo :: Funcao -> Double -> Double -> Maybe Double
encontrarMinimo funcao x1 x2 = let 
                                passos = 10000
                                passo = (x2 - x1) / passos
                                valoresX = [x1 + passo * i | i <- [0..passos]]
                                valoresY = [(avaliarFuncao funcao x) | x <- valoresX]
                            in if null valoresY then Nothing else Just (minimum valoresY)

calcularComprimentoCurva :: Funcao -> Double -> Double -> Comprimento
calcularComprimentoCurva (Funcao (Linear a b) _) x1 x2 = let
                                                            y1 = avaliarFuncao (Funcao (Linear a b) "") x1
                                                            y2 = avaliarFuncao (Funcao (Linear a b) "") x2
                                                        in sqrt ((x2 - x1)^2 + (y2 - y1)^2)
calcularComprimentoCurva funcao x1 x2 = (0.5 * x2 * sqrt (1 + 4 * x2 ^ 2) + 0.25 * log (2 * x2 + sqrt (1 + 4 * x2 ^ 2))) - (0.5 * x1 * sqrt (1 + 4 * x1 ^ 2) + 0.25 * log (2 * x1 + sqrt (1 + 4 * x1 ^ 2)))

