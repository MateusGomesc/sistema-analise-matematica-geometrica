# Guia de intalação local do projeto

Este projeto é um sistema desenvolvido em Haskell como parte da disciplina de Programação Funcional. Ele fornece um conjunto de ferramentas para realizar cálculos de Engenharia, Cálculo, Geometria e Álgebra Linear, além de gerenciar um banco de dados de projetos.

## Pré-requisitos

Para compilar e executar este projeto, você precisará do seguinte:

1.  **GHC (The Glasgow Haskell Compiler)**
2.  **Cabal (Haskell build tool)**
3.  Git

A maneira mais fácil e recomendada de instalar e gerenciar as versões do GHC e do Cabal é através do **[GHCup](https://www.ghcup.haskell.org/)**. Siga as instruções no site oficial para instalá-lo no seu sistema. Para realizar a intalção do Git siga o **[Guia de intalçao do Git](https://git-scm.com/book/pt-br/v2/Come%C3%A7ando-Instalando-o-Git)**

## Instalação

Siga estes passos para configurar o ambiente e compilar o projeto:

1.  **Clone o repositório:**
    ```bash
    git clone git@github.com:MateusGomesc/sistema-analise-matematica-geometrica.git
    cd sistema-analise-matematica-geometrica
    ```

2.  **Configure a Versão Correta do GHC:**
    Este projeto foi desenvolvido com a versão **GHC 9.12.2** para garantir a compatibilidade com as dependências listadas no arquivo `.cabal`. Use o GHCup para instalar e ativar esta versão:
    ```bash
    # Instala o GHC 9.12.2 (se ainda não o tiver)
    ghcup install ghc 9.12.2

    # Define o GHC 9.12.2 como a versão ativa
    ghcup set ghc 9.12.2
    ```

3.  **Atualize o índice de pacotes do Cabal:**
    É uma boa prática garantir que o Cabal conheça os pacotes mais recentes.
    ```bash
    cabal update
    ```

4.  **Compile o projeto:**
    Este comando irá baixar todas as dependências necessárias e compilar o código-fonte.
    ```bash
    cabal build
    ```
    Isso pode levar alguns minutos na primeira vez.

## Execução

Após a compilação bem-sucedida, você pode executar o programa principal com o seguinte comando:

```bash
# O nome 'projeto-engenharia' é o nome do executável definido no arquivo .cabal
cabal run projeto-engenharia
```
Isso iniciará a interface interativa do sistema no seu terminal.
