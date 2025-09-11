module Engineering.Electrical where

import Types

tensaoOhm :: Corrente -> Resistencia-> Tensao
tensaoOhm i r = i * r

potenciaEletricaRI :: Resistencia -> Corrente-> PotenciaEletrica
potenciaEletricaRI r i = r * i^2

potenciaEletricaVR :: Tensao -> Resistencia-> PotenciaEletrica
potenciaEletricaVR v r = v^2 / r

resistenciaSerie :: [Resistencia] -> Resistencia
resistenciaSerie [] = 0
resistenciaSerie (r:rs) = r + resistenciaSerie rs

resistenciaParalelo :: [Resistencia] -> Resistencia
resistenciaParalelo [] = 0  
resistenciaParalelo [r] = r
resistenciaParalelo (r:rs) = 1 / (1/r + 1 / resistenciaParalelo rs)

impedanciaAC :: Resistencia -> Reatancia -> Impedancia
impedanciaAC r x = sqrt (r^2 + x^2)

polarParaRetangular :: Double -> Angulo -> (Double, Double)
polarParaRetangular r theta = (x, y)
  where
    x = r * cos theta
    y = r * sin theta
    
retangularParaPolar :: Double -> Double -> (Double, Double)
retangularParaPolar x y = (r, theta)
  where
    r = sqrt (x^2 + y^2)
    theta = atan2 y x

potenciaEletricaVI :: Tensao -> Corrente -> PotenciaEletrica
potenciaEletricaVI v i = v * i
