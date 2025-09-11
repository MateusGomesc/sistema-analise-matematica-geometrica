module Engineering.Civil where

import Types
=======
module Engineering.Civil where

import Types

volumeConcreto :: Figura -> Volume
volumeConcreto (Paralelepipedo c l a) = c * l * a
volumeConcreto (Cilindro r h)         = pi * r^2 * h
volumeConcreto (Esfera r)             = (4/3) * pi * r^3
volumeConcreto _                      = 0  -- Outros casos não aplicáveis
