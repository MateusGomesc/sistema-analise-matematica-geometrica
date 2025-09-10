# sistema-analise-matematica-geometrica

### Modelo para documentar função

1. Copie o texto abaixo
2. Subistitua {assinatura da função} pela assinatura real da função Ex: Soma :: Int -> Int -> Int
3. Coloque a descrição


```haskell
assinatura da função
```

Descrição...

## Diagrama hierárquico do menu



## Funções

### Especialista 2: Álgebra Linear e Operações com Matrizes

```haskell
somarMatrizes :: Matriz -> Matriz -> Maybe Matriz
```

Recebe duas matrizes do tipo algébrico Matriz. <br/>
Verifica se ambas possuem as mesmas dimensões. <br/>
Realiza a soma elemento a elemento entre as duas matrizes. <br/>
Retorna Just Matriz com o resultado da soma ou Nothing se forem incompatíveis. <br/>

```haskell
dimensoesIguais :: [[Double]] -> [[Double]] -> Bool
```

Recebe duas listas de listas de Double representando matrizes. <br/>
Verifica se ambas possuem o mesmo número de linhas e colunas. <br/>
Retorna True se forem compatíveis ou False caso contrário.<br/>

```haskell
multiplicarMatrizes :: Matriz -> Matriz -> Maybe Matriz
```

Recebe duas matrizes do tipo algébrico Matriz. <br/>
Verifica se o número de colunas da primeira é igual ao número de linhas da segunda. <br/>
Realiza a multiplicação matricial utilizando transposição e produto escalar. <br/>
Retorna Just Matriz com o resultado ou Nothing se forem incompatíveis. <br/>

```haskell
transpostaMatriz :: Matriz -> Matriz
```

Recebe uma matriz do tipo algébrico Matriz. <br/>
Aplica a transposição da matriz, trocando linhas por colunas. <br/>
Retorna uma nova Matriz transposta.<br/>

```haskell
determinante :: Matriz -> Maybe Double
```

Recebe uma matriz do tipo algébrico Matriz. <br/>
Verifica se a matriz é quadrada. <br/>
Calcula o determinante utilizando expansão de Laplace.<br/>
Retorna Just Double com o valor do determinante ou Nothing se a matriz não for quadrada. <br/>

```haskell
resolverSistemaLinear :: Matriz -> Vetor -> Maybe Vetor
```

Recebe uma matriz quadrada do tipo Matriz e um vetor de termos independentes do tipo Vetor. <br/>
Verifica se o sistema é compatível e se a matriz é invertível. <br/>
Resolve o sistema linear utilizando o método de Cramer. <br/>
Retorna Just Vetor com a solução ou Nothing se não houver solução única. <br/>

```haskell
produtoEscalar :: Vetor -> Vetor -> Maybe Double
```

Recebe dois vetores do tipo algébrico Vetor. <br/>
Verifica se os vetores possuem o mesmo tamanho. <br/>
Calcula o produto escalar entre os dois vetores. <br/>
Retorna Just Double com o resultado ou Nothing se forem incompatíveis. <br/>

```haskell
normaVetor :: Vetor -> Double
```

Recebe um vetor do tipo algébrico Vetor. <br/>
Calcula a norma do vetor (comprimento). <br/>
Retorna um Double representando a norma. <br/>

```haskell
anguloEntreVetores :: Vetor -> Vetor -> Maybe Angulo
```

Recebe dois vetores do tipo algébrico Vetor. <br/>
Verifica se os vetores são compatíveis e não nulos. <br/>
Calcula o ângulo entre os vetores usando produto escalar e norma. <br/>
Retorna Just Angulo em radianos ou Nothing se não for possível calcular. <br/>

### Especialista 3: Cálculo Diferencial e Integral

```haskell
avaliarFuncao :: Funcao -> Double -> Double
```

Recebe um tipo algebrico Funcao representando a função desejada. <br/>
Recebe um Double representado o valor que será aplicado na função. <br/>
Verifica qual função está sendo recebida pelo tipo algebrico, aplica o valor recebido de acordo com essa verificação. <br/>
Devolve um Double representando o valor da função com a aplicação desejada. <br/>

```haskell
derivadaNumerica :: Funcao -> Double -> Double
```

Recebe um tipo algebrico Funcao representando a função que deseja derivar.<br/>
Recebe um Double para calcular o valor da derivada naquele ponto. <br/>
Verifica qual foi o tipo algebrico foi recebido e calcula a derivada no ponto solicitado. <br/>
Retorna um Double representando o valor da derivada. <br/>

```haskell
integralNumerica :: Funcao -> Double -> Double -> Int -> Double
```

Recebe um tipo algebrico Funcao representando a função a integrar. <br/>
Recebe, na sequência, dois Doubles representado os limites de integração. <br/>
Recebe um Int representando a precisão da inetgral. <br/>
Verifica qual foi o tipo algebrico recebido e calcula a integral aplicando os limites de integração seguindo o Teorema Fundamental do Cálculo. <br/>
Retorna um Double representando o valor da integral.<br/>

```haskell
encontrarRaizes :: Funcao -> Double -> Double -> [Double]
```

Recebe uma função. <br/>
Recebe dois doubles indicando o intervalo no eixo x. <br/>
Verifica qual função chegou, faz o calculo da raíz e verifica intervalo. <br/>
Devolve uma lista contendo as raízes encontradas, lista vazia se não for encontrada nenhuma raíz. <br/>

```haskell
encontrarMaximo :: Funcao -> Double -> Double -> Maybe Double
```

Recebe uma função. <br/>
Recebe dois Doubles que indicam o intervalo no eixo x. <br/>
Separa o intervalo em vários pontos contínuos, acha o valor de todos esses pontos e encontra o maior dentre eles. (Quanto maior a divisão maior a precisão do resultado). <br/>
Retorna Just x onde x é o valor máximo da entrada ou Nothing se não houver máximo. <br/>

```haskell
encontrarMinimo :: Funcao -> Double -> Double -> Maybe Double
```
Recebe uma função. <br/>
Recebe dois Doubles que indicam o intervalo no eixo x. <br/>
Separa o intervalo em vários pontos contínuos, acha o valor de todos esses pontos e encontra o menor dentre eles. (Quanto maior a divisão maior a precisão do resultado). <br/>
Retorna Just x onde x é o valor minímo da entrada ou Nothing se não houver minímo. <br/>

```haskell
calcularComprimentoCurva :: Funcao -> Double -> Double -> Comprimento
```

Recebe uma função. <br/>
Recebe dois Doubles indicando o intervalo no eixo x. <br/>
Para funções lineares utiliza da hipotenusa para calcular, para funções quadráticas utiliza de integrais e derivadas. <br/>
Retorna o valor do tamanho da curva. <br/>

### Especialista 4: Algoritmos e Estruturas de Dados

```haskell
quickSort :: (Ord a) => [a] -> [a]
```
Recebe uma lista de elementos de um tipo que pode ser ordenado (Ord a => [a]). <br/>
Verifica se a lista está vazia; se não, seleciona o primeiro elemento como pivô. <br/>
Divide os elementos restantes em duas listas: menores ou iguais ao pivô e maiores que o pivô. <br/>
Aplica recursivamente o QuickSort em cada sublista. <br/>
Devolve uma lista ordenada do mesmo tipo ([a]) em ordem crescente.  <br/>

```haskell
mergeSort :: (Ord a) => [a] -> [a]
```
Recebe uma lista de elementos de um tipo que pode ser ordenado (Ord a => [a]). <br/>
Verifica se a lista está vazia ou contém apenas um elemento; se sim, retorna a lista como está. <br/>
Caso contrário, divide a lista em duas metades (left e right). <br/>
Aplica recursivamente o MergeSort em cada metade. <br/>
Une as duas metades ordenadas usando a função merge, comparando elemento a elemento. <br/>
Devolve uma lista ordenada do mesmo tipo ([a]) em ordem crescente. <br/>

```haskell
insertionSort :: (Ord a) => [a] -> [a]
```
Recebe uma lista de elementos de um tipo que pode ser ordenado (Ord a => [a]). <br/>
Verifica se a lista está vazia; se sim, retorna a lista como está. <br/>
Caso contrário, pega o primeiro elemento (x) e aplica recursivamente o InsertionSort no restante da lista. <br/>
Insere o elemento x na posição correta da lista ordenada usando a função auxiliar insert. <br/>
Devolve uma lista ordenada do mesmo tipo ([a]) em ordem crescente. <br/>

```haskell
 buscarProjeto :: Int -> [Projeto] -> Maybe Projeto
```
Recebe um identificador do tipo 'Int' representando o ID do projeto a ser buscado. <br/>
Recebe, na sequência, uma lista de projetos '[Projeto]'. <br/>
Percorre a lista verificando se algum projeto possui o ID igual ao fornecido. <br/>
Retorna 'Just Projeto' caso encontre o projeto correspondente. <br/>
Retorna 'Nothing' caso nenhum projeto com o ID informado seja encontrado. <br/>

```haskell
inserirOrdenado :: (Ord a) => a -> [a] -> [a]
```


### Engenharia Civil

```haskell
volumeConcreto :: Figura -> Volume
```

Recebe uma figura geométrica do tipo algébrico Figura. <br/>
Verifica qual tipo de figura foi recebida. <br/>
Calcula o volume de concreto necessário com base na fórmula da figura. <br/>
Retorna um Volume em metros cúbicos. <br/>

### Engenharia Mecânica

```haskell
calcularTorque :: Forca -> Distancia -> Angulo -> Torque
```

Recebe uma força (Forca), uma distância (Distancia) e um ângulo (Angulo). <br/>
Aplica a fórmula do torque: τ = F · r · sin(θ). <br/>
Retorna um Torque em Newton-metro. <br/>

```haskell
velocidadeAngular :: Velocidade -> Raio -> VelocidadeAngular
```

Recebe uma velocidade linear (Velocidade) e um raio (Raio). <br/>
Aplica a fórmula da velocidade angular: ω = v / r. <br/>
Retorna uma VelocidadeAngular em rad/s. Retorna 0 se o raio for nulo. <br/>

```haskell
aceleracaocentripeta :: Velocidade -> Raio -> Aceleracao
```

Recebe uma velocidade tangencial (Velocidade) e um raio (Raio). <br/>
Aplica a fórmula da aceleração centrípeta: a = v² / r. <br/>
Retorna uma Aceleracao em m/s². Retorna 0 se o raio for nulo. <br/>

```haskell
energiaCinetica :: Massa-> Velocidade-> Energia
```

Recebe uma tipo algebrico Massa e um tipo algebrico Velocidade. <br/>
Realiza o cálculo. <br/>
Retorna um tipo algebrico Energia com o resultado. <br/>

```haskell
energiaPotencial :: Massa-> Altura-> Energia
```

Recebe uma tipo algebrico Massa e um tipo algebrico Altura. <br/>
Realiza o cálculo. <br/>
Retorna um tipo algebrico Energia com o resultado. <br/>

```haskell
centroMassaX :: [(Massa, Distancia)]-> Distancia
```

Recebe uma lista contendo tuplas com o tipo algebrico Massa na primeira posição e na segunda posição o tipo algebrico Distancia. <br/>
Realiza o cálculo. <br/>
Retorna um tipo algebrico Distancia com o resultado. <br/>

### Engenharia Elétrica

```haskell
tensaoOhm :: Corrente-> Resistencia-> Tensao
```

Recebe uma tipo algebrico Corrente e um tipo algebrico Resistência. <br/>
Realiza o cálculo. <br/>
Retorna um tipo algebrico Tensao com o resultado. <br/>

```haskell
potenciaEletricaRI :: Resistencia -> Corrente -> PotenciaEletrica
```
Recebe um tipo algebrico Resistencia e um tipo algebrico Corrente. <br/>
Realiza o cálculo. <br/>
Retorna um tipo algebrico PotenciaEletrica com o resultado. <br/> 
 
```haskell
potenciaEletricaVR :: Tensao -> Resistencia -> PotenciaEletrica
```
Recebe um tipo algebrico Tensao e um tipo algebrico Resistencia. <br/>
Realiza o cálculo. <br/>
Retorna um tipo algebrico PotenciaEletrica com o resultado. <br/>

```haskell
resistenciaSerie :: [Resistencia] -> Resistencia
```
Recebe uma lista de resistências ([Resistencia]). <br/>
Realiza o cálculo da resistência equivalente de resistores ligados em série. <br/>
Retorna um tipo algebrico Resistencia com o resultado. <br/>

```haskell
resistenciaParalelo :: [Resistencia] -> Resistencia
```
Recebe uma lista de resistências ([Resistencia]). <br/>
Realiza o cálculo da resistência equivalente de resistores ligados em paralelo. <br/>
Retorna um tipo algebrico Resistencia com o resultado. <br/>
