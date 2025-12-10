module Expansion.DataDLC where

import Labs2025 (Mapa, Posicao, Direcao, VidaMinhoca, TipoArma, Ticks, NumMinhoca)
import Tarefa0_geral ()

-- ? Ideias -> Areia | Lava | Lodo | Wind
-- | Um tipo de terreno do mapa.
data TerrenosDLC 
    -- | Terreno vazio.
    = Ar
    -- | Terreno que afoga minhocas.
    | Agua
    -- | Terreno opaco e destrutivel.
    | Terra
    -- | Terreno opaco e indestrutivel.
    | Pedra
    deriving (Eq,Ord,Show,Read,Enum)

-- | Os diversos tipos de arma disponíveis para uma minhoca. --? Ideias -> Kamehameha , Air Strike
data TipoArmaDLC = Jetpack | Escavadora | Bazuca | Mina | Dinamite
    deriving (Eq,Ord,Show,Read,Enum)

-- | O estado completo de uma minhoca.
data MinhocaDLC = Minhoca
    -- | Uma posição no mapa. Opcional porque a minhoca pode ter saído do mapa.
    { posicaoMinhoca :: Maybe Posicao

    , vidaMinhoca :: VidaMinhoca -- | O estado de saúde da minhoca.
    -- | Munições de @Jetpack@.
    , jetpackMinhoca :: Int
    -- | Munições de @Escavadora@.
    , escavadoraMinhoca :: Int
    -- | Munições de @Bazuca@.
    , bazucaMinhoca :: Int
    -- | Munições de @Mina@.
    , minaMinhoca :: Int
    -- | Munições de @Dinamite@.
    , dinamiteMinhoca :: Int
    
    -- ? EQUIPA DA MINHOCA
    }
    deriving (Eq,Ord,Show,Read)


data ObjetoDLC
    -- | Um disparo de uma arma.
    = Disparo
        -- | A posição do disparo no mapa.
        { posicaoDisparo :: Posicao
        -- | A direção do disparo.
        , direcaoDisparo :: Direcao
        -- | O tipo de arma do disparo.
        , tipoDisparo :: TipoArma
        -- | O tempo até o disparo explodir. Opcional porque nem todos os disparos de todas as armas têm um tempo pré-definido para explodir.
        , tempoDisparo :: Maybe Ticks
        -- | A minhoca que efetuou o disparo.
        , donoDisparo :: NumMinhoca
        }
    -- | Um barril de pólvora.
    | Barril
        -- | A posição do barril no mapa.
        { posicaoBarril :: Posicao
        -- | Se o barril está prestes a explodir ou não.
        , explodeBarril :: Bool
        }
    | HealthPack  -- todo add
        
        -- | A posicao do HP no mapa
        { posicaoHP :: Posicao
        -- | Cura que o HP da
        , quantCura :: Int

        }
    deriving (Eq,Ord,Show,Read)

-- | Estado do jogo.
data EstadoDLC = Estado
    -- | O mapa atual.
    { mapaEstado :: Mapa
    -- | Uma lista com os objetos presentes no mapa. Para as funções que vai desenvolver, deve considerar que a ordem dos elementos é irrelevante.
    , objetosEstado :: [ObjetoDLC]
    -- | Uma lista com as minhocas no jogo. A ordem dos elementos é relevante, no sentido cada minhoca vai ser identificada pelo seu índice na lista.
    , minhocasEstado :: [MinhocaDLC]
    
    }
    deriving (Eq,Ord,Show,Read)

-- | Uma jogada que uma minhoca pode efetuar.
data JogadaDLC
    -- | Disparar uma arma numa dada direção.
    = Dispara TipoArma Direcao
    -- | Mover-se numa dada direção.
    | Move Direcao
    -- | Ficar parado no local
    | Parado
    deriving (Eq,Ord,Show,Read)
