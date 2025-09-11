module Engineering.Mechanical where

import Types

energiaCinetica :: Massa -> Velocidade-> Energia
=======
-- Engenharia Mecânica
calcularTorque :: Forca -> Distancia -> Angulo -> Torque
calcularTorque f r theta = f * r * sin theta

velocidadeAngular :: Velocidade -> Raio -> VelocidadeAngular
velocidadeAngular v r
  | r == 0    = 0          -- divisão por zero
  | otherwise = v / r

aceleracaocentripeta :: Velocidade -> Raio -> Aceleracao
aceleracaocentripeta v r
  | r == 0    = 0           -- evita divisão por zero
  | otherwise = v^2 / r

energiaCinetica :: Massa-> Velocidade-> Energia
energiaCinetica m v = 0.5 * m * v^2

energiaPotencial :: Massa -> Altura-> Energia
energiaPotencial m h = m * 9.81 * h

centroMassaX :: [(Massa, Distancia)]-> Distancia
centroMassaX dados = let
                        numerador = map (\(m, d) -> m * d) dados
                        denominador = map (\(m, _) -> m) dados
                    in (sum numerador) / (sum denominador)
