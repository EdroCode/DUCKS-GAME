module DataDLC where

import Labs2025(Ticks, NumMinhoca, Direcao, Posicao)



-- | Uma matriz é um conjunto de elementos a duas dimensões.
--
-- Em notação matemática, é geralmente representada por:
--
-- <<https://haslab.github.io/Teaching/LI1/2526/img/matriz.png>>
type Matriz a = [[a]]


-- | A dimensão de uma matrix dada como um par (/número de linhas/,/número de colunhas/).
type Dimensao = (Int,Int)



data Team = Red | Blue 
    deriving (Eq,Ord,Show,Read,Enum)

data VidaMinhocaDLC
    -- | Está viva com um número inteiros de pontos de vida.
    = VivaDLC Int
    -- | Está morta.
    | MortaDLC
    deriving (Eq,Ord,Show,Read)


type MapaDLC = Matriz TerrenoDLC
-- | O estado completo de uma minhoca.
data MinhocaDLC = MinhocaDLC
    -- | Uma posição no mapa. Opcional porque a minhoca pode ter saído do mapa.
    { posicaoMinhocaDLC :: Maybe Posicao

    , vidaMinhocaDLC :: VidaMinhocaDLC -- | O estado de saúde da minhoca.
    -- | Munições de @Jetpack@.
    , jetpackMinhocaDLC :: Int
    -- | Munições de @Escavadora@.
    , escavadoraMinhocaDLC :: Int
    -- | Munições de @Bazuca@.
    , bazucaMinhocaDLC :: Int
    -- | Munições de @Mina@.
    , minaMinhocaDLC :: Int
    -- | Munições de @Dinamite@.
    , dinamiteMinhocaDLC :: Int
    -- | Munições de @FlameTrower@.
    , flameMinhocaDLC :: Int
    -- | O estado de burning da minhoca
    , burningCounter :: Int -- o Default dado por terrenos de lava é 5 mas pode variar, default flametrower é 2
    -- | Equipa da Minhoca (default é Nothing)
    , equipaMinhoca :: Maybe Team
    }
    deriving (Eq,Ord,Show,Read)



-- | Um tipo de terreno do mapa.
data TerrenoDLC
    -- | Terreno vazio.
    = ArDLC
    -- | Terreno que afoga minhocas.
    | AguaDLC
    -- | Terreno opaco e destrutivel.
    | TerraDLC
    -- | Terreno opaco e indestrutivel.
    | PedraDLC
    -- | Terreno que queima minhocas
    | Lava

    deriving (Eq,Ord,Show,Read,Enum)

-- | Os diversos tipos de arma disponíveis para uma minhoca.
data TipoArmaDLC = JetpackDLC | EscavadoraDLC | BazucaDLC | MinaDLC | DinamiteDLC | FlameTrower
    deriving (Eq,Ord,Show,Read,Enum)

-- | Um objeto colocado no mapa.
data ObjetoDLC
    -- | Um disparo de uma arma.
    = DisparoDLC
        -- | A posição do disparo no mapa.
        { posicaoDisparoDLC :: Posicao
        -- | A direção do disparo.
        , direcaoDisparoDLC :: Direcao
        -- | O tipo de arma do disparo.
        , tipoDisparoDLC :: TipoArmaDLC
        -- | O tempo até o disparo explodir. Opcional porque nem todos os disparos de todas as armas têm um tempo pré-definido para explodir.
        , tempoDisparoDLC :: Maybe Ticks
        -- | A minhoca que efetuou o disparo.
        , donoDisparoDLC :: NumMinhoca
        }
    -- | Um barril de pólvora.
    | BarrilDLC
        -- | A posição do barril no mapa.
        { posicaoBarrilDLC :: Posicao
        -- | Se o barril está prestes a explodir ou não.
        , explodeBarrilDLC :: Bool
        }
    -- | Um pacote medico que cura a minhoca.
    | HealthPack
        -- | A posicao do HP no mapa
        { posicaoHP :: Posicao
        -- | A quantidade de cura do HP
        , curaHP :: Int  -- default 50
        }
    | AmmoPack
        -- | A posicao do AP no mapa
        { posicaoAP :: Posicao
        -- | A quantidade de cura do HP
        , ammoGiven :: Int  -- default 5
        -- | O tipo de munição dada
        , ammoType :: TipoArmaDLC
        }
    deriving (Eq,Ord,Show,Read)

-- | Uma jogada que uma minhoca pode efetuar.
data JogadaDLC
    -- | Disparar uma arma numa dada direção.
    = Dispara TipoArmaDLC Direcao
    -- | Mover-se numa dada direção.
    | Move Direcao

    deriving (Eq,Ord,Show,Read)


-- | Estado do jogo.
data EstadoDLC = EstadoDLC
    -- | O mapa atual.
    { mapaEstadoDLC :: MapaDLC
    -- | Uma lista com os objetos presentes no mapa. Para as funções que vai desenvolver, deve considerar que a ordem dos elementos é irrelevante.
    , objetosEstadoDLC :: [ObjetoDLC]
    -- | Uma lista com as minhocas no jogo. A ordem dos elementos é relevante, no sentido cada minhoca vai ser identificada pelo seu índice na lista.
    , minhocasEstadoDLC :: [MinhocaDLC]
    -- | A arma selecionada pela minhoca
    , armaSelecionada :: Maybe TipoArmaDLC
    -- | O indice da minhoca selecionada
    , minhocaSelecionada :: NumMinhoca
    }
    deriving (Eq,Ord,Show,Read)

-- | Devolve a 'Posição' de um 'Objeto'.(Utilizado na função 'existeBarril')
posicaoObjeto :: ObjetoDLC -> Posicao
posicaoObjeto d@(DisparoDLC {})  = posicaoDisparoDLC d
posicaoObjeto b@(BarrilDLC {}) = posicaoBarrilDLC b
posicaoObjeto hp@(HealthPack {}) = posicaoHP hp
posicaoObjeto hp@(AmmoPack {}) = posicaoAP hp



fireDamage :: Int
fireDamage = 10
