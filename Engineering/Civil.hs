module Engineering.Civil where

import Types

momentoInerciaRetangular :: Largura -> Altura -> MomentoInercia
momentoInerciaRetangular b h = (b * h^3) / 12

tensaoNormal :: Forca -> Area -> Pressao
tensaoNormal f a = f / a


deflexaoViga :: Forca -> Comprimento -> ModuloElasticidade -> MomentoInercia -> Distancia
deflexaoViga f l e i = (f * l^3) / (48 * e * i)


cargaCriticaEuler :: ModuloElasticidade -> MomentoInercia -> Comprimento -> Forca
cargaCriticaEuler e i l = (pi^2 * e * i) / (l^2)

volumeConcreto :: Figura -> Volume
volumeConcreto (Paralelepipedo c l a) = c * l * a
volumeConcreto (Cilindro r h)         = pi * r^2 * h
volumeConcreto (Esfera r)             = (4/3) * pi * r^3
volumeConcreto _                      = 0  -- Outros casos não aplicáveis
