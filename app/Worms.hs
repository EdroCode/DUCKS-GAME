module Worms where

import Labs2025

-- | Estado usado pela interface Gloss: pode estar no menu, a jogar ou em estado de saída.
data Worms
	= Menu Int          -- ^ Menu com opção actualmente seleccionada (0..n)
	| BotSimulation Estado Float Int    -- ^ Estado do jogo + accumulator (segundos) + tick counter
	| FreeRoam Estado Float Int Jogada
    | Help              -- ^ Tela de ajuda (mostra instruções)
	| Quit              -- ^ Estado de saída (mostrado apenas)
	deriving (Eq, Show)


-- | Estado de exemplo usado pelo menu para iniciar a demo do jogo.
novoEstado :: Estado
novoEstado = Estado
    { mapaEstado =
        [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Terra,Terra,Ar,Ar,Ar,Terra,Terra,Terra,Terra]
        ,[Ar,Ar,Ar,Ar,Pedra,Agua,Agua,Agua,Agua,Agua,Pedra,Terra,Terra,Terra,Ar,Ar,Terra,Terra,Terra,Terra]
        ,[Ar,Ar,Terra,Ar,Pedra,Pedra,Agua,Agua,Agua,Agua,Pedra,Terra,Terra,Terra,Ar,Ar,Terra,Terra,Terra,Terra]
        ,[Terra,Terra,Terra,Terra,Terra,Pedra,Pedra,Pedra,Agua,Agua,Pedra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Terra]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Terra,Terra]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra]
        ]
    , objetosEstado =
        [Barril {posicaoBarril = (1,16), explodeBarril = False}
        ,Disparo {posicaoDisparo = (0,17), tipoDisparo = Dinamite, direcaoDisparo = Norte, donoDisparo = 2, tempoDisparo = Just 3}
        ,Disparo {posicaoDisparo = (1,18), tipoDisparo = Mina, direcaoDisparo = Este, donoDisparo = 0, tempoDisparo = Nothing}
        ]
    , minhocasEstado =
        [Minhoca {posicaoMinhoca = Just (4,3), vidaMinhoca = Viva 70, jetpackMinhoca = 1, escavadoraMinhoca = 6, bazucaMinhoca = 0, minaMinhoca = 1, dinamiteMinhoca = 0}
        ,Minhoca {posicaoMinhoca = Just (4,0), vidaMinhoca = Viva 60, jetpackMinhoca = 1, escavadoraMinhoca = 9, bazucaMinhoca = 0, minaMinhoca = 1, dinamiteMinhoca = 0}
        ,Minhoca {posicaoMinhoca = Just (7,6), vidaMinhoca = Viva 100, jetpackMinhoca = 1, escavadoraMinhoca = 6, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 2}
        ,Minhoca {posicaoMinhoca = Just (1,11), vidaMinhoca = Viva 100, jetpackMinhoca = 1, escavadoraMinhoca = 6, bazucaMinhoca = 4, minaMinhoca = 1, dinamiteMinhoca = 3}
        ]
    }

flatWorld :: Estado
flatWorld = Estado
    { mapaEstado =
        [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra]
        ]
    , objetosEstado =
        []
    , minhocasEstado =
        [Minhoca {posicaoMinhoca = Just (2,4), vidaMinhoca = Viva 100, jetpackMinhoca = 1, escavadoraMinhoca = 0, bazucaMinhoca = 0, minaMinhoca = 0, dinamiteMinhoca = 0}
        ]
    }