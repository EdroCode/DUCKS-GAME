module JogadasArquivo where

import Labs2025
import Tarefa0_2025
import Tarefa0_geral
import EstadosArquivo


-- Estado base, minhoca normal
estadoBase = estado1valido

-- Minhoca no ar
estadoNoAr = Estado
    { mapaEstado = mapaOk
    , objetosEstado = []
    , minhocasEstado =
        [Minhoca { posicaoMinhoca = Just (3,0)  -- assume acima do chão
                 , vidaMinhoca = Viva 100
                 , jetpackMinhoca = 1
                 , escavadoraMinhoca = 1
                 , bazucaMinhoca = 1
                 , minaMinhoca = 1
                 , dinamiteMinhoca = 1
                 }]
    }

-- Minhoca na borda norte (fora do mapa)
estadoBordaNorte = Estado
    { mapaEstado = mapaOk
    , objetosEstado = []
    , minhocasEstado =
        [Minhoca { posicaoMinhoca = Just (0,5)
                 , vidaMinhoca = Viva 100
                 , jetpackMinhoca = 1
                 , escavadoraMinhoca = 1
                 , bazucaMinhoca = 1
                 , minaMinhoca = 1
                 , dinamiteMinhoca = 1
                 }]
    }

-- Minhoca à beira da água
estadoProxAgua = Estado
    { mapaEstado = mapaOk
    , objetosEstado = []
    , minhocasEstado =
        [Minhoca { posicaoMinhoca = Just (4,8) -- perto do bloco de água
                 , vidaMinhoca = Viva 100
                 , jetpackMinhoca = 1
                 , escavadoraMinhoca = 1
                 , bazucaMinhoca = 1
                 , minaMinhoca = 1
                 , dinamiteMinhoca = 1
                 }]
    }

movimentosTeste =
    [ (1 :: Int, Move Norte, estadoBase)
    , (0, Move Sul, estadoBase)
    , (0 :: Int, Move Este, estadoBase)
    , (0 :: Int, Move Oeste, estadoBase)
    , (0 :: Int, Move Nordeste, estadoBase)
    , (1 :: Int, Move Noroeste, estadoBase)
    , (1 :: Int, Move Sudeste, estadoBase)
    , (0 :: Int, Move Sudoeste, estadoBase)
    , (0 :: Int, Move Norte, estadoNoAr)       
    , (0 :: Int, Move Norte, estadoBordaNorte) 
    , (0 :: Int, Move Este, estadoProxAgua)    
    ]



disparosTeste =
    -- Jetpack
    [ (1 :: Int, Dispara Jetpack Norte, estadoBase)
    , (0 :: Int, Dispara Jetpack Nordeste, estadoNoAr)
    , (0 :: Int, Dispara Jetpack Noroeste, estadoBordaNorte)
    -- Escavadora
    , (1 :: Int, Dispara Escavadora Sul, estadoBase)
    , (0 :: Int, Dispara Escavadora Oeste, estadoBase) -- assume Pedra
    , (0 :: Int, Dispara Escavadora Norte, estadoBordaNorte)
    -- Bazuca
    , (1 :: Int, Dispara Bazuca Sul, estadoBase)
    , (0 :: Int, Dispara Bazuca Noroeste, estadoBordaNorte)
    -- Mina
    , (1 :: Int, Dispara Mina Sul, estadoBase)
    , (0 :: Int, Dispara Mina Oeste, estadoBase)       -- assume ocupado
    , (0 :: Int, Dispara Mina Noroeste, estadoBordaNorte)
    -- Dinamite
    , (0 :: Int, Dispara Dinamite Sul, estadoBase)
    , (1 :: Int, Dispara Dinamite Oeste, estadoBase)  -- assume ocupado
    , (0 :: Int, Dispara Dinamite Noroeste, estadoBordaNorte)
    ]



estado1valido = Estado{mapaEstado=mapaOk, objetosEstado=[disparoValido1, disparoValido2, barrilValido1], minhocasEstado=[minhocaValida1, minhocaValida2]} 
estado2valido = Estado{mapaEstado=mapaOk, objetosEstado=[disparoValido1, disparoValido2, barrilValido1], minhocasEstado=[minhocaValida1]} 




jogadasTeste = disparosTeste ++ movimentosTeste