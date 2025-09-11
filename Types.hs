module Types where
import Data.Time.Calendar

-- Dist^ancias e comprimentos
type Distancia = Double -- metros (m)
type Comprimento = Double -- metros (m)
type Altura = Double -- metros (m)
type Largura = Double -- metros (m)
type Espessura = Double -- metros (m)
type Raio = Double -- metros (m)
type Diametro = Double -- metros (m)

-- Areas e volumes ´
type Area = Double -- metros quadrados (m²)
type Volume = Double -- metros c´ubicos (m³)
type Perimetro = Double -- metros (m)

-- Angulos ^
type Angulo = Double -- radianos (rad)
type AnguloGraus = Double -- graus (°)

-- For¸cas e momentos
type Forca = Double -- Newtons (N)
type Momento = Double -- Newton-metro (N§·§m)
type Torque = Double -- Newton-metro (N§·§m)
type Pressao = Double -- Pascal (Pa) ou MPa

-- Massas e densidades
type Massa = Double -- quilogramas (kg)
type Densidade = Double -- kg/m³
type Peso = Double -- Newtons (N)

-- Velocidades e acelera¸c~oes
type Velocidade = Double -- metros por segundo (m/s)
type Aceleracao = Double -- metros por segundo² (m/s²)
type VelocidadeAngular = Double -- radianos por segundo (rad/s)

-- Energias
type Energia = Double -- Joules (J)
type Potencia = Double -- Watts (W)
type Trabalho = Double -- Joules (J)

-- Grandezas b´asicas
type Tensao = Double -- Volts (V)
type Corrente = Double -- Amp`eres (A)
type Resistencia = Double -- Ohms (§Ω§)
type Capacitancia = Double -- Farads (F)
type Indutancia = Double -- Henrys (H)

-- Pot^encias el´etricas
type PotenciaEletrica = Double -- Watts (W)
type PotenciaReativa = Double -- VAR
type PotenciaAparente = Double -- VA

-- Imped^ancias
type Impedancia = Double -- Ohms (§Ω§)
type Reatancia = Double -- Ohms (§Ω§)
type FatorPotencia = Double -- adimensional (0 a 1)

-- Propriedades mec^anicas
type ModuloElasticidade = Double -- Pascal (Pa)
type ResistenciaTracao = Double -- Pascal (Pa)
type ResistenciaCompressao = Double -- Pascal (Pa)
type TensaoCisalhamento = Double -- Pascal (Pa)
type FatorSeguranca = Double -- adimensional

-- Propriedades t´ermicas
type Temperatura = Double -- Celsius (°C) ou Kelvin (K)
type CondutividadeTermica = Double -- W/(m§·§K)
type CalorEspecifico = Double -- J/(kg§·§K)

-- Propriedades geom´etricas
type MomentoInercia = Double -- §m^4§
type ModuloResistencia = Double -- m³
type RaioGiracao = Double -- metros (m)

-- Custos e valores
type Custo = Double -- Reais (R)
type Preco = Double -- Reais por unidade (R/unidade)
type Orcamento = Double -- Reais (R)
type CustoUnitario = Double -- Reais por unidade (R/unidade)

-- Tempos
type Tempo = Double -- segundos (s)
type Duracao = Double -- horas (h)
type Prazo = Int -- dias

data Status = Planejamento | EmDesenvolvimento | EmRevisao | Concluido | Cancelado
    deriving (Show, Eq, Ord, Read)

data TipoProjeto = Civil | Mecanica | Eletrica | Estrutural
    deriving (Show, Eq, Read)

data Ponto2D = Ponto2D Distancia Distancia 
    deriving (Show, Eq, Read)

data Ponto3D = Ponto3D Distancia Distancia Distancia 
    deriving (Show, Eq, Read)

data Figura = Retangulo Largura Altura
    | Circulo Raio
    | Triangulo Ponto2D Ponto2D Ponto2D
    | Poligono [Ponto2D]
    | Esfera Raio
    | Cilindro Raio Altura
    | Paralelepipedo Comprimento Largura Altura
    deriving (Show, Eq, Read)

data Matriz = Matriz [[Double]] 
    deriving (Show, Eq, Read)

data Vetor = Vetor [Double] 
    deriving (Show, Eq, Read)

data TipoFuncao = Linear Double Double -- ax + b
    | Quadratica Double Double Double -- ax² + bx + c
    | Exponencial Double Double -- a * e^(bx)
    | Logaritmica Double Double -- a * ln(bx)
    | Trigonometrica TipoTrig Double Double -- a * func(bx)
    deriving (Show, Eq, Read, Ord)

data TipoTrig = Seno | Cosseno | Tangente 
    deriving (Show, Eq, Read, Ord)

data Funcao = Funcao TipoFuncao String 
    deriving (Show, Eq, Read, Ord) -- Tipo e descri¸c~ao

data Projeto = Projeto
    { idProjeto :: Int
    , nomeProjeto :: String
    , tipoProjeto :: TipoProjeto
    , status :: Status
    , coordenadas :: [Ponto3D] -- Pontos do projeto
    , figuras :: [Figura] -- Formas geom´etricas
    , materiais :: [Material] -- Materiais utilizados
    , calculos :: [CalculoMat] -- C´alculos realizados
    , funcoes :: [Funcao] -- Fun¸c~oes matem´aticas
    , dataInicio :: Day
    , dataFim :: Maybe Day
    , orcamento :: Orcamento
    } deriving (Show, Eq, Read)

data Material = Material
    { nomeMaterial :: String
    , densidade :: Densidade
    , resistencia :: ResistenciaTracao
    , custo :: CustoUnitario
    , quantidade :: Volume
    } deriving (Show, Eq, Read)

data CalculoMat = CalculoMat
    { nomeCalculo :: String
    , entrada :: [Double]
    , resultado :: Double
    , unidade :: String
    , formula :: String
    } deriving (Show, Eq, Read)

-- Tipo para cargas estruturais
data Carga = CargaPontual Forca Ponto2D
    | CargaDistribuida Forca Comprimento -- for¸ca por metro
    | CargaTriangular Forca Forca Comprimento -- carga vari´avel
    deriving (Show, Eq, Read)

-- Tipo para propriedades de se¸c~oes
data SecaoTransversal = SecaoRetangular Largura Altura
    | SecaoCircular Raio
    | SecaoI Altura Largura Espessura Espessura -- h, b, tw, tf
    deriving (Show, Eq, Read)

data ArvoreBinaria a = Vazia | No a (ArvoreBinaria a) (ArvoreBinaria a)
    deriving (Show, Eq, Read)
