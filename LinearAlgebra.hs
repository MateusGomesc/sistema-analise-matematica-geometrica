module LinearAlgebra where

import Types

somarMatrizes :: Matriz -> Matriz -> Maybe Matriz
somarMatrizes (Matriz a) (Matriz b)
  | dimensoesIguais a b = Just (Matriz (zipWith (zipWith (+)) a b))
  | otherwise             = Nothing

-- Verifica se duas matrizes têm as mesmas dimensões
dimensoesIguais :: [[Double]] -> [[Double]] -> Bool
dimensoesIguais [] [] = True
dimensoesIguais (x:xs) (y:ys)
  | length x /= length y = False
  | otherwise            = dimensoesIguais xs ys
dimensoesIguais _ _ = False

multiplicarMatrizes :: Matriz -> Matriz -> Maybe Matriz
multiplicarMatrizes (Matriz []) _ = Nothing
multiplicarMatrizes _ (Matriz []) = Nothing
multiplicarMatrizes (Matriz a) (Matriz b)
  | length (head a) /= length b = Nothing
  | otherwise = Just (Matriz resultado)
  where
    Matriz bTransposta = transpostaMatriz (Matriz b)
    resultado = [ [ sum (zipWith (*) linhaA colunaB) | colunaB <- bTransposta ] | linhaA <- a ]

transpostaMatriz :: Matriz -> Matriz
transpostaMatriz (Matriz m) = Matriz (transpor m)
  where
    transpor [] = []
    transpor ([]:_) = []
    transpor x = map head x : transpor (map tail x)

determinante :: Matriz -> Maybe Double
determinante (Matriz [[n]]) = Just n
determinante (Matriz [[a, b], [c, d]]) = Just (a * d - b * c)
determinante (Matriz [[a11, a12, a13],
                      [a21, a22, a23],
                      [a31, a32, a33]]) =
  let pos = a11*a22*a33 + a12*a23*a31 + a13*a21*a32
      neg = a13*a22*a31 + a11*a23*a32 + a12*a21*a33
  in Just (pos - neg)
determinante _ = Nothing

-- Resolve sistema linear de equacoes usando Cramer
resolverSistemaLinear :: Matriz -> Vetor -> Maybe Vetor
resolverSistemaLinear (Matriz a) (Vetor b)
  | not (quadrada a) || length a /= length b = Nothing
  | otherwise =
      case determinante (Matriz a) of
        Nothing -> Nothing
        Just detA
          | detA == 0 -> Nothing
          | otherwise -> Just (Vetor [ determinanteColuna i detA | i <- [0..n-1] ])
  where
    n = length a
    quadrada m = all (\linha -> length linha == n) m

    determinanteColuna i detA =
      case determinante (Matriz (matrizComColunaSubstituida a b i)) of
        Just d  -> d / detA
        Nothing -> 0  -- fallback seguro, embora não devesse ocorrer

    matrizComColunaSubstituida :: [[Double]] -> [Double] -> Int -> [[Double]]
    matrizComColunaSubstituida m v colIndex =
      [ take colIndex linha ++ [v !! i] ++ drop (colIndex + 1) linha | (i, linha) <- zip [0..] m ]


produtoEscalar :: Vetor -> Vetor -> Maybe Double
produtoEscalar (Vetor v1) (Vetor v2)
  | length v1 /= length v2 = Nothing
  | otherwise = Just (sum (zipWith (*) v1 v2))

normaVetor :: Vetor -> Double
normaVetor (Vetor v) = sqrt (sum (map (^2) v))

anguloEntreVetores :: Vetor -> Vetor -> Maybe Angulo
anguloEntreVetores v1 v2 =
  case produtoEscalar v1 v2 of
    Nothing -> Nothing
    Just p
      | normaV1 == 0 || normaV2 == 0 -> Nothing
      | otherwise -> Just (acos (p / (normaV1 * normaV2)))
  where
    normaV1 = normaVetor v1
    normaV2 = normaVetor v2



