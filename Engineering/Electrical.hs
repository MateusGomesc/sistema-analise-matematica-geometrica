module Engineering.Electrical where

import Types

tensaoOhm :: Corrente-> Resistencia-> Tensao
tensaoOhm i r = i * r