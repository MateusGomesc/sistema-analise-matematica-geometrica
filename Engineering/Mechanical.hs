module Engineering.Mechanical where

import Types

energiaCinetica :: Massa -> Velocidade-> Energia
energiaCinetica m v = 0.5 * m * v^2

energiaPotencial :: Massa -> Altura-> Energia
energiaPotencial m h = m * 9.81 * h

centroMassaX :: [(Massa, Distancia)]-> Distancia
centroMassaX dados = let
                        numerador = map (\(m, d) -> m * d) dados
                        denominador = map (\(m, _) -> m) dados
                    in (sum numerador) / (sum denominador)