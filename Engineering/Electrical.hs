module Engineering.Electrical where

import Types

tensaoOhm :: Corrente-> Resistencia-> Tensao
tensaoOhm i r = i * r

potenciaEletricaRI :: Resistencia-> Corrente-> PotenciaEletrica
potenciaEletricaRI r i = r * i^2

potenciaEletricaVR :: Tensao-> Resistencia-> PotenciaEletrica
potenciaEletricaVR v r = v^2 / r


