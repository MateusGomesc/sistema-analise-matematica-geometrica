# sistema-analise-matematica-geometrica

### Modelo para documentar função

1. Copie o texto abaixo
2. Subistitua {assinatura da função} pela assinatura real da função Ex: Soma :: Int -> Int -> Int
3. Coloque a descrição


```haskell
assinatura da função
```

Descrição...


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