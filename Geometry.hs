-- 1. Distância entre dois pontos 2D
distanciaEntrePontos :: Ponto2D -> Ponto2D -> Distancia
distanciaEntrePontos (Ponto2D x1 y1) (Ponto2D x2 y2) =
    sqrt ((x2 - x1)**2 + (y2 - y1)**2)

-- 2. Distância entre dois pontos 3D
distancia3D :: Ponto3D -> Ponto3D -> Distancia
distancia3D (Ponto3D x1 y1 z1) (Ponto3D x2 y2 z2) =
    sqrt ((x2 - x1)**2 + (y2 - y1)**2 + (z2 - z1)**2)

-- 3. Ponto médio entre dois pontos 2D
pontoMedio :: Ponto2D -> Ponto2D -> Ponto2D
pontoMedio (Ponto2D x1 y1) (Ponto2D x2 y2) =
    Ponto2D ((x1 + x2)/2) ((y1 + y2)/2)

-- 4. Área de figuras
calcularArea :: Figura -> Area
calcularArea (Retangulo w h) = w * h
calcularArea (Circulo r) = pi * r^2
calcularArea (Triangulo p1 p2 p3) =
    let a = distanciaEntrePontos p1 p2
        b = distanciaEntrePontos p2 p3
        c = distanciaEntrePontos p3 p1
        s = (a + b + c) / 2
    in sqrt (s * (s - a) * (s - b) * (s - c)) -- Fórmula de Heron
calcularArea (Poligono pts) =
    let xs = map (\(Ponto2D x _) -> x) pts
        ys = map (\(Ponto2D _ y) -> y) pts
        shiftedYs = tail ys ++ [head ys]
        shiftedXs = tail xs ++ [head xs]
    in 0.5 * abs (sum (zipWith (*) xs shiftedYs) - sum (zipWith (*) ys shiftedXs))
calcularArea (Cilindro r h) = 2 * pi * r * h + 2 * pi * r^2
calcularArea (Esfera r) = 4 * pi * r^2
calcularArea (Paralelepipedo l w h) = 2 * (l*w + l*h + w*h)

-- 5. Perímetro de figuras 2D
calcularPerimetro :: Figura -> Perimetro
calcularPerimetro (Retangulo w h) = 2 * (w + h)
calcularPerimetro (Circulo r) = 2 * pi * r
calcularPerimetro (Triangulo p1 p2 p3) =
    distanciaEntrePontos p1 p2 + distanciaEntrePontos p2 p3 + distanciaEntrePontos p3 p1
calcularPerimetro (Poligono pts) =
    let ptsCiclo = pts ++ [head pts]
    in sum $ zipWith distanciaEntrePontos ptsCiclo (tail ptsCiclo)
calcularPerimetro _ = 0 -- Sólidos 3D não têm perímetro definido diretamente

-- 6. Volume de sólidos
calcularVolume :: Figura -> Volume
calcularVolume (Cilindro r h) = pi * r^2 * h
calcularVolume (Esfera r) = (4/3) * pi * r^3
calcularVolume (Paralelepipedo l w h) = l * w * h
calcularVolume _ = 0 -- Figuras 2D não têm volume

-- 7. Teste se ponto está dentro de um polígono (algoritmo ray-casting)
dentroDoPoligono :: Ponto2D -> [Ponto2D] -> Bool
dentroDoPoligono (Ponto2D x y) pts =
    let ptsCiclo = pts ++ [head pts]
        cruzamentos = length [() |
            (Ponto2D x1 y1, Ponto2D x2 y2) <- zip ptsCiclo (tail ptsCiclo),
            (y1 > y) /= (y2 > y),
            x < (x2 - x1) * (y - y1) / (y2 - y1) + x1
            ]
    in odd cruzamentos


 -- 8. Interseção de duas retas (retas definidas por dois pontos cada)
intersecaoRetas :: (Ponto2D, Ponto2D) -> (Ponto2D, Ponto2D) -> Maybe Ponto2D
intersecaoRetas (Ponto2D x1 y1, Ponto2D x2 y2) (Ponto2D x3 y3, Ponto2D x4 y4) =
    let denom = (x1-x2)*(y3-y4) - (y1-y2)*(x3-x4)
    in if denom == 0
       then Nothing -- retas paralelas
       else
           let px = ((x1*y2 - y1*x2)*(x3-x4) - (x1-x2)*(x3*y4 - y3*x4)) / denom
               py = ((x1*y2 - y1*x2)*(y3-y4) - (y1-y2)*(x3*y4 - y3*x4)) / denom
           in Just (Ponto2D px py)
