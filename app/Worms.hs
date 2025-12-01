module Worms where

import Labs2025

-- | Estado usado pela interface Gloss: pode estar no menu, a jogar ou em estado de saída.
data Worms
	= Menu Int          -- ^ Menu com opção actualmente seleccionada (0..n)
	| Playing Estado Float Int    -- ^ Estado do jogo + accumulator (segundos) + tick counter
	| Help              -- ^ Tela de ajuda (mostra instruções)
	| Quit              -- ^ Estado de saída (mostrado apenas)
	deriving (Eq, Show)


-- | Estado de exemplo usado pelo menu para iniciar a demo do jogo.
novoEstado :: Estado
novoEstado = Estado
	    { mapaEstado =
        [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Ar,Pedra,Agua,Agua,Agua,Agua,Agua]
        ,[Terra,Terra,Terra,Terra,Pedra,Pedra,Agua,Agua,Agua,Agua]
        ,[Terra,Terra,Terra,Terra,Terra,Pedra,Pedra,Pedra,Agua,Agua]
        ]
    , objetosEstado =
        []
    , minhocasEstado =
        [Minhoca {posicaoMinhoca = Just (2,1), vidaMinhoca = Viva 70, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 0}
        ,Minhoca {posicaoMinhoca = Just (1,4), vidaMinhoca = Viva 50, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 0}
        ]
	}
