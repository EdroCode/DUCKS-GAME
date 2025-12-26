module Worms where

import Labs2025(VidaMinhoca(Viva, Morta), TipoArma(Dinamite, Mina, Jetpack, Escavadora, Bazuca), Direcao(Norte,Este,Oeste,Sul,Nordeste,Noroeste,Sudoeste,Sudeste),tempoDisparo,donoDisparo,direcaoDisparo, posicaoBarril, tipoDisparo, explodeBarril, posicaoDisparo,Mapa, Objeto(Barril, Disparo),Estado,Posicao, Terreno(Agua, Ar, Terra, Pedra),Estado(Estado), Minhoca (Minhoca), Jogada, NumMinhoca, NumObjeto, minhocasEstado, objetosEstado, mapaEstado, posicaoMinhoca, vidaMinhoca, jetpackMinhoca, escavadoraMinhoca, bazucaMinhoca, minaMinhoca, dinamiteMinhoca)
import Control.Exception (tryJust)
import Tarefa4 (canFollowVoid)
import DataDLC( JogadaDLC,TerrenoDLC(ArDLC, TerraDLC, PedraDLC, AguaDLC), Team(Blue,Red), EstadoDLC, jetpackMinhocaDLC, escavadoraMinhocaDLC, bazucaMinhocaDLC, minaMinhocaDLC, dinamiteMinhocaDLC,mapaEstadoDLC, objetosEstadoDLC, minhocasEstadoDLC,posicaoHP,armaSelecionada,minhocaSelecionada,curaHP, equipaMinhoca, burningCounter, Matriz, fireDamage, VidaMinhocaDLC(MortaDLC, VivaDLC), MapaDLC,EstadoDLC(EstadoDLC), MinhocaDLC (MinhocaDLC), ObjetoDLC (AmmoPack, posicaoAP, ammoGiven, ammoType), TerrenoDLC(Lava, AguaDLC, ArDLC, TerraDLC, PedraDLC), posicaoMinhocaDLC, vidaMinhocaDLC, burningCounter, posicaoDisparoDLC, direcaoDisparoDLC, tempoDisparoDLC, tipoDisparoDLC, donoDisparoDLC, posicaoBarrilDLC, explodeBarrilDLC, ObjetoDLC, ObjetoDLC(DisparoDLC, BarrilDLC, HealthPack), TipoArmaDLC(MinaDLC, BazucaDLC, DinamiteDLC))


-- | Estado usado pela interface Gloss: pode estar no menu, a jogar ou em estado de saída.
data Worms
	= Menu Int          -- ^ Menu com opção actualmente seleccionada (0..n)
	| BotSimulation Estado Float Int (NumMinhoca, Jogada)    -- ^ Estado do jogo + accumulator (segundos) + tick counter + última jogada
	| PVP EstadoDLC Float Int JogadaDLC
    | MapCreatorTool EstadoDLC Int Int Int-- estado blocoselected modo selecaoalternativa
    | MapSelector
    | LevelSelector Int
    | Help              -- ^ Tela de ajuda (mostra instruções)
	| Quit              -- ^ Estado de saída (mostrado apenas)
	| GameOver Team         -- ^ Tela de fim de jogo PVP
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
        ,Disparo {posicaoDisparo = (0,10), tipoDisparo = Dinamite, direcaoDisparo = Norte, donoDisparo = 2, tempoDisparo = Just 3}
        ,Disparo {posicaoDisparo = (1,18), tipoDisparo = Mina, direcaoDisparo = Este, donoDisparo = 0, tempoDisparo = Nothing}
        ]
    , minhocasEstado =
        [Minhoca {posicaoMinhoca = Just (4,3), vidaMinhoca = Viva 70, jetpackMinhoca = 1, escavadoraMinhoca = 6, bazucaMinhoca = 0, minaMinhoca = 1, dinamiteMinhoca = 0}
        ,Minhoca {posicaoMinhoca = Just (4,0), vidaMinhoca = Viva 60, jetpackMinhoca = 1, escavadoraMinhoca = 9, bazucaMinhoca = 0, minaMinhoca = 1, dinamiteMinhoca = 0}
        ,Minhoca {posicaoMinhoca = Just (7,6), vidaMinhoca = Viva 100, jetpackMinhoca = 1, escavadoraMinhoca = 6, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 2}
        ,Minhoca {posicaoMinhoca = Just (1,11), vidaMinhoca = Viva 100, jetpackMinhoca = 1, escavadoraMinhoca = 6, bazucaMinhoca = 4, minaMinhoca = 1, dinamiteMinhoca = 3}
        ]
        }

flatWorld :: EstadoDLC
flatWorld = EstadoDLC
    { mapaEstadoDLC =
        [[ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC]
        ,[ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC]
        ,[ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC]
        ,[ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC]
        ,[ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC]
        ,[ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC]
        ,[ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC]
        ,[TerraDLC,AguaDLC,AguaDLC,TerraDLC,TerraDLC,TerraDLC,TerraDLC,TerraDLC,TerraDLC,TerraDLC,Lava,ArDLC,ArDLC,ArDLC]
        ,[TerraDLC,TerraDLC,TerraDLC,TerraDLC,TerraDLC,TerraDLC,TerraDLC,TerraDLC,TerraDLC,TerraDLC,TerraDLC,TerraDLC,TerraDLC,TerraDLC]
        ]
    , objetosEstadoDLC =
        [BarrilDLC{posicaoBarrilDLC = (4,2), explodeBarrilDLC = False},
        HealthPack {posicaoHP = (2,3), curaHP = 40},
        AmmoPack {posicaoAP = (2,4), ammoGiven = 50, ammoType = BazucaDLC}]
    , minhocasEstadoDLC =
        [MinhocaDLC {posicaoMinhocaDLC = Just (0,4), vidaMinhocaDLC = VivaDLC 100, jetpackMinhocaDLC = 100, escavadoraMinhocaDLC = 100, bazucaMinhocaDLC = 100, minaMinhocaDLC = 100, dinamiteMinhocaDLC = 100, burningCounter = 0, equipaMinhoca = Just Red}
        ,MinhocaDLC {posicaoMinhocaDLC = Just (0,7), vidaMinhocaDLC = VivaDLC 100, jetpackMinhocaDLC = 100, escavadoraMinhocaDLC = 100, bazucaMinhocaDLC = 100, minaMinhocaDLC = 100, dinamiteMinhocaDLC = 100, burningCounter = 0, equipaMinhoca = Just Blue}
        ]
    , armaSelecionada = Nothing
    , minhocaSelecionada = 0
    }