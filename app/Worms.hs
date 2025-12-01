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
sampleEstado :: Estado
sampleEstado = Estado
	    { mapaEstado =
		    [ [Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
		    , [Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
		    , [Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
		    , [Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
		    , [Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
		    , [Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
		    , [Terra,Terra,Terra,Terra,Terra,Terra,Terra,Agua,Agua,Agua,Agua,Agua]
		    , [Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Terra,Terra,Terra,Terra,Terra,Agua]
		    ]
	, objetosEstado =
			[ Barril { posicaoBarril = (5,7), explodeBarril = False } ]
	, minhocasEstado =
			[ Minhoca { posicaoMinhoca = Just (5,1), vidaMinhoca = Viva 100, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1 }
			, Minhoca { posicaoMinhoca = Just (5,4), vidaMinhoca = Viva 40, jetpackMinhoca = 2, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 0 }
			, Minhoca { posicaoMinhoca = Just (5,8), vidaMinhoca = Morta, jetpackMinhoca = 0, escavadoraMinhoca = 0, bazucaMinhoca = 0, minaMinhoca = 0, dinamiteMinhoca = 0 }
			]
	}
