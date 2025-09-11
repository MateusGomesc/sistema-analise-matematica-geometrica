# Exemplos de uso

### Retângulo 5x10
```haskell
show $ calcularArea (Retangulo 5 10)
-- Saída: 50.0
```

### Círculo raio 3
```haskell
show $ calcularArea (Circulo 3)
-- Saída: 28.274333882308138
```

### Triângulo com pontos (0,0), (4,0), (0,3)
```haskell
show $ calcularArea (Triangulo (Ponto2D 0 0) (Ponto2D 4 0) (Ponto2D 0 3))
-- Saída: 6.0
```

### Polígono quadrado com vértices (0,0), (4,0), (4,4), (0,4)
```haskell
show $ calcularArea (Poligono [Ponto2D 0 0, Ponto2D 4 0, Ponto2D 4 4, Ponto2D 0 4])
-- Saída: 16.0
```

### Projeto de Fundação Residencial

```haskell
projetoFundacao :: Projeto
projetoFundacao = Projeto
  { idProjeto = 101
  , nomeProjeto = "Fundação Residencial"
  , tipoProjeto = Civil
  , statusProjeto = Planejamento
  , etapas = ["Estudo do solo", "Dimensionamento", "Execução"]
  , riscos = ["Solo instável", "Alta umidade"]
  , materiais =
      [ Material "Concreto FCK30" 2400 30e6 350.0 120.0
      , Material "Aço CA-50" 7850 500e6 4.50 80.0
      ]
  , calculos =
      [ CalculoMat "Volume de concreto" [10, 12, 0.3] 36.0 "m³" "V = C * L * A"
      ]
  , funcoes =
      [ Funcao (Linear 2 3) "f(x) = 2x + 3"
      ]
  , dataInicio = fromGregorian 2025 3 1
  , dataFim = Just $ fromGregorian 2025 6 30
  , orcamento = 85000.0
  }
```

### Projeto de Reforma Comercial

```haskell
projetoReforma :: Projeto
projetoReforma = Projeto
  { idProjeto = 102
  , nomeProjeto = "Reforma Comercial"
  , tipoProjeto = Civil
  , statusProjeto = Execucao
  , etapas = ["Demolição", "Reforço estrutural", "Acabamento"]
  , riscos = ["Interferência elétrica", "Desvio de cronograma"]
  , materiais =
      [ Material "Concreto FCK25" 2350 25e6 330.0 90.0
      , Material "Aço CA-60" 7900 600e6 5.00 60.0
      , Material "Argamassa" 2000 5e6 120.0 50.0
      ]
  , calculos =
      [ CalculoMat "Área de reforço" [5, 2] 10.0 "m²" "A = b * h"
      , CalculoMat "Carga admissível" [5000, 2.5] 2000.0 "kgf" "P = σ * A"
      ]
  , funcoes =
      [ Funcao (Quadratica 1 (-2) 1) "f(x) = x² - 2x + 1"
      , Funcao (Exponencial 1 0.5) "f(x) = e^(0.5x)"
      ]
  , dataInicio = fromGregorian 2025 2 15
  , dataFim = Just $ fromGregorian 2025 7 15
  , orcamento = 120000.0
  }
```

### Comparação entre os projetos

```haskell
--Aplicando:
putStrLn $ compararProjetos projetoFundacao projetoReforma

--Saída esperada (resumida):
==========================================
      COMPARAÇÃO DE PROJETOS
==========================================
ID: 101 vs 102 (Diferentes)
Nome: Fundação Residencial vs Reforma Comercial (Diferentes)
Tipo: Civil vs Civil (Iguais)
Status: Planejamento vs Execucao (Diferentes)
Orçamento: 85000.00 vs 120000.00 (Diferentes)
Data de Início: 2025-03-01 vs 2025-02-15 (Diferentes)
Data de Fim: 2025-06-30 vs 2025-07-15 (Diferentes)

-- Detalhes de Complexidade --
Materiais: 2 vs 3 (Diferentes)
Cálculos: 1 vs 2 (Diferentes)
Funções: 1 vs 2 (Diferentes)
==========================================
```
