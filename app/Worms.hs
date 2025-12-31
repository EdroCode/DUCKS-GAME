{-# OPTIONS_GHC -Wno-unused-imports #-}
{-|
Module      : Worms
Description : Definição do estado do jogo Worms com suporte a temas.
-}
module Worms where

import Labs2025(VidaMinhoca(Viva, Morta), TipoArma(Dinamite, Mina, Jetpack, Escavadora, Bazuca), Direcao(Norte,Este,Oeste,Sul,Nordeste,Noroeste,Sudoeste,Sudeste),tempoDisparo,donoDisparo,direcaoDisparo, posicaoBarril, tipoDisparo, explodeBarril, posicaoDisparo,Mapa, Objeto(Barril, Disparo),Estado,Posicao, Terreno(Agua, Ar, Terra, Pedra),Estado(Estado), Minhoca (Minhoca), Jogada, NumMinhoca, NumObjeto, minhocasEstado, objetosEstado, mapaEstado, posicaoMinhoca, vidaMinhoca, jetpackMinhoca, escavadoraMinhoca, bazucaMinhoca, minaMinhoca, dinamiteMinhoca)
import DataDLC( TipoArmaDLC(JetpackDLC, EscavadoraDLC, BazucaDLC, MinaDLC, DinamiteDLC, FlameTrower),JogadaDLC,TerrenoDLC(ArDLC, TerraDLC, PedraDLC, AguaDLC, Gelo), Team(Blue,Red), EstadoDLC,danosEstado, jetpackMinhocaDLC, escavadoraMinhocaDLC, bazucaMinhocaDLC, minaMinhocaDLC, dinamiteMinhocaDLC,mapaEstadoDLC, objetosEstadoDLC, minhocasEstadoDLC,posicaoHP,armaSelecionada,minhocaSelecionada,curaHP, equipaMinhoca, burningCounter, Matriz, fireDamage, VidaMinhocaDLC(MortaDLC, VivaDLC), MapaDLC,EstadoDLC(EstadoDLC), MinhocaDLC (MinhocaDLC, flameMinhocaDLC, ultimaDirecaoHorizontal),  TerrenoDLC(Lava, AguaDLC, ArDLC, TerraDLC, PedraDLC), posicaoMinhocaDLC, vidaMinhocaDLC, burningCounter, posicaoDisparoDLC, direcaoDisparoDLC, tempoDisparoDLC, tipoDisparoDLC, donoDisparoDLC, posicaoBarrilDLC, explodeBarrilDLC, ObjetoDLC(DisparoDLC, BarrilDLC, HealthPack,AmmoPack, posicaoAP, ammoGiven, ammoType), TipoArmaDLC(MinaDLC, BazucaDLC, DinamiteDLC, FlameTrower), Dimensao)


-- | Tipo para identificar os temas disponíveis
data TemaAtual = TemaBase | TemaNatal
    deriving (Eq, Show, Read, Enum)


-- | Estado usado pela interface Gloss: pode estar no menu, a jogar ou em estado de saída.
data Worms
    = Menu Int TemaAtual
    | BotSimulation Estado Float Int (NumMinhoca, Jogada) TemaAtual   
    | PVP EstadoDLC Float Int JogadaDLC TemaAtual            
    | MapCreatorTool EstadoDLC Int Int Int Int Bool (Maybe Int) MinhocaDLC ObjetoDLC TemaAtual 
    | MapSelector TemaAtual                                  
    | LevelSelector Int [EstadoDLC] TemaAtual                
    | Help Int TemaAtual                                     
    | Quit Int TemaAtual                                     
    | GameOver Team TemaAtual                                
    | ThemesMenu TemaAtual
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


level1 :: EstadoDLC
level1 = EstadoDLC {mapaEstadoDLC = [[TerraDLC,TerraDLC,PedraDLC,PedraDLC,PedraDLC,TerraDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC],
                                    [TerraDLC,TerraDLC,TerraDLC,TerraDLC,TerraDLC,TerraDLC,TerraDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC],[ArDLC,ArDLC,TerraDLC,TerraDLC,TerraDLC,TerraDLC,TerraDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC],[ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,TerraDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC],[ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC],[TerraDLC,TerraDLC,TerraDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,PedraDLC,AguaDLC,AguaDLC],[TerraDLC,TerraDLC,TerraDLC,TerraDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,PedraDLC,PedraDLC,AguaDLC,AguaDLC],[TerraDLC,TerraDLC,TerraDLC,TerraDLC,TerraDLC,ArDLC,ArDLC,TerraDLC,TerraDLC,PedraDLC,PedraDLC,PedraDLC,AguaDLC,AguaDLC],[TerraDLC,TerraDLC,TerraDLC,TerraDLC,TerraDLC,TerraDLC,TerraDLC,TerraDLC,TerraDLC,PedraDLC,PedraDLC,PedraDLC,PedraDLC,AguaDLC]], objetosEstadoDLC = [], minhocasEstadoDLC = [MinhocaDLC {posicaoMinhocaDLC = Just (4,0), vidaMinhocaDLC = VivaDLC 100, jetpackMinhocaDLC = 100, escavadoraMinhocaDLC = 100, bazucaMinhocaDLC = 100, minaMinhocaDLC = 100, dinamiteMinhocaDLC = 100, flameMinhocaDLC = 100, burningCounter = 0, equipaMinhoca = Just Blue, ultimaDirecaoHorizontal = Oeste},MinhocaDLC {posicaoMinhocaDLC = Just (4,11), vidaMinhocaDLC = VivaDLC 100, jetpackMinhocaDLC = 100, escavadoraMinhocaDLC = 100, bazucaMinhocaDLC = 100, minaMinhocaDLC = 100, dinamiteMinhocaDLC = 100, flameMinhocaDLC = 100, burningCounter = 0, equipaMinhoca = Just Red, ultimaDirecaoHorizontal = Oeste},MinhocaDLC {posicaoMinhocaDLC = Just (0,6), vidaMinhocaDLC = VivaDLC 100, jetpackMinhocaDLC = 100, escavadoraMinhocaDLC = 100, bazucaMinhocaDLC = 100, minaMinhocaDLC = 100, dinamiteMinhocaDLC = 100, flameMinhocaDLC = 100, burningCounter = 0, equipaMinhoca = Just Red, ultimaDirecaoHorizontal = Oeste},MinhocaDLC {posicaoMinhocaDLC = Just (7,6), vidaMinhocaDLC = VivaDLC 100, jetpackMinhocaDLC = 100, escavadoraMinhocaDLC = 100, bazucaMinhocaDLC = 100, minaMinhocaDLC = 100, dinamiteMinhocaDLC = 100, flameMinhocaDLC = 100, burningCounter = 0, equipaMinhoca = Just Blue, ultimaDirecaoHorizontal = Oeste}], armaSelecionada = Nothing, minhocaSelecionada = 0, danosEstado = []}

level2 :: EstadoDLC
level2 = EstadoDLC {mapaEstadoDLC = [[ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC],[ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC],[ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC],[ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC],[ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,PedraDLC,PedraDLC,PedraDLC,PedraDLC],[TerraDLC,PedraDLC,PedraDLC,Lava,Lava,ArDLC,ArDLC,Lava,Lava,PedraDLC,PedraDLC,PedraDLC,PedraDLC,TerraDLC],[TerraDLC,TerraDLC,PedraDLC,PedraDLC,Lava,Lava,Lava,Lava,Lava,PedraDLC,PedraDLC,PedraDLC,TerraDLC,TerraDLC],[TerraDLC,TerraDLC,PedraDLC,PedraDLC,Lava,Lava,Lava,Lava,Lava,PedraDLC,PedraDLC,TerraDLC,TerraDLC,TerraDLC],[TerraDLC,TerraDLC,TerraDLC,PedraDLC,Lava,Lava,Lava,Lava,Lava,PedraDLC,PedraDLC,TerraDLC,TerraDLC,TerraDLC]], objetosEstadoDLC = [BarrilDLC {posicaoBarrilDLC = (4,0), explodeBarrilDLC = False},HealthPack {posicaoHP = (3,0), curaHP = 50},AmmoPack {posicaoAP = (3,13), ammoGiven = 50, ammoType = JetpackDLC},DisparoDLC {posicaoDisparoDLC = (0,0), direcaoDisparoDLC = Este, tipoDisparoDLC = FlameTrower, tempoDisparoDLC = Nothing, donoDisparoDLC = 0},DisparoDLC {posicaoDisparoDLC = (1,13), direcaoDisparoDLC = Oeste, tipoDisparoDLC = FlameTrower, tempoDisparoDLC = Nothing, donoDisparoDLC = 0}], minhocasEstadoDLC = [MinhocaDLC {posicaoMinhocaDLC = Just (4,2), vidaMinhocaDLC = VivaDLC 100, jetpackMinhocaDLC = 100, escavadoraMinhocaDLC = 100, bazucaMinhocaDLC = 100, minaMinhocaDLC = 100, dinamiteMinhocaDLC = 100, flameMinhocaDLC = 100, burningCounter = 0, equipaMinhoca = Just Blue, ultimaDirecaoHorizontal = Oeste},MinhocaDLC {posicaoMinhocaDLC = Just (3,10), vidaMinhocaDLC = VivaDLC 100, jetpackMinhocaDLC = 100, escavadoraMinhocaDLC = 100, bazucaMinhocaDLC = 100, minaMinhocaDLC = 100, dinamiteMinhocaDLC = 100, flameMinhocaDLC = 100, burningCounter = 0, equipaMinhoca = Just Red, ultimaDirecaoHorizontal = Oeste}], armaSelecionada = Nothing, minhocaSelecionada = 0, danosEstado = []}

level3 :: EstadoDLC
level3 = EstadoDLC {mapaEstadoDLC = [[ArDLC,ArDLC,ArDLC,ArDLC,PedraDLC,TerraDLC,TerraDLC,TerraDLC,TerraDLC,TerraDLC,TerraDLC,PedraDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC],[ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,PedraDLC,TerraDLC,TerraDLC,TerraDLC,TerraDLC,PedraDLC,PedraDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC],[Gelo,Gelo,Gelo,Gelo,ArDLC,PedraDLC,PedraDLC,TerraDLC,TerraDLC,PedraDLC,PedraDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,PedraDLC],[Gelo,Gelo,Gelo,Gelo,ArDLC,ArDLC,PedraDLC,TerraDLC,PedraDLC,PedraDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,PedraDLC,PedraDLC],[Gelo,Gelo,Gelo,Gelo,ArDLC,ArDLC,PedraDLC,PedraDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,PedraDLC],[Gelo,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,PedraDLC,PedraDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC],[Gelo,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC],[Gelo,ArDLC,ArDLC,Gelo,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,PedraDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC],[Gelo,ArDLC,Gelo,Gelo,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,PedraDLC,TerraDLC,TerraDLC,TerraDLC,TerraDLC,TerraDLC,PedraDLC,Lava,Lava,Lava],[Gelo,Gelo,Gelo,Gelo,AguaDLC,AguaDLC,AguaDLC,AguaDLC,AguaDLC,TerraDLC,TerraDLC,TerraDLC,TerraDLC,TerraDLC,TerraDLC,PedraDLC,Lava,Lava,Lava],[Gelo,Gelo,Gelo,Gelo,AguaDLC,AguaDLC,AguaDLC,AguaDLC,AguaDLC,TerraDLC,TerraDLC,TerraDLC,TerraDLC,TerraDLC,PedraDLC,Lava,Lava,Lava,Lava],[Gelo,Gelo,Gelo,Gelo,AguaDLC,AguaDLC,AguaDLC,AguaDLC,AguaDLC,TerraDLC,TerraDLC,TerraDLC,PedraDLC,PedraDLC,Lava,Lava,Lava,Lava,Lava]], objetosEstadoDLC = [AmmoPack {posicaoAP = (7,11), ammoGiven = 50, ammoType = MinaDLC},AmmoPack {posicaoAP = (7,2), ammoGiven = 50, ammoType = BazucaDLC},HealthPack {posicaoHP = (6,10), curaHP = 50},HealthPack {posicaoHP = (6,3), curaHP = 50},HealthPack {posicaoHP = (1,1), curaHP = 50},HealthPack {posicaoHP = (1,0), curaHP = 50}], minhocasEstadoDLC = [MinhocaDLC {posicaoMinhocaDLC = Just (8,1), vidaMinhocaDLC = VivaDLC 100, jetpackMinhocaDLC = 5, escavadoraMinhocaDLC = 5, bazucaMinhocaDLC = 2, minaMinhocaDLC = 1, dinamiteMinhocaDLC = 1, flameMinhocaDLC = 20, burningCounter = 0, equipaMinhoca = Just Blue, ultimaDirecaoHorizontal = Oeste},MinhocaDLC {posicaoMinhocaDLC = Just (7,15), vidaMinhocaDLC = VivaDLC 100, jetpackMinhocaDLC = 5, escavadoraMinhocaDLC = 5, bazucaMinhocaDLC = 2, minaMinhocaDLC = 1, dinamiteMinhocaDLC = 1, flameMinhocaDLC = 20, burningCounter = 0, equipaMinhoca = Just Red, ultimaDirecaoHorizontal = Oeste},MinhocaDLC {posicaoMinhocaDLC = Just (1,18), vidaMinhocaDLC = VivaDLC 100, jetpackMinhocaDLC = 5, escavadoraMinhocaDLC = 5, bazucaMinhocaDLC = 2, minaMinhocaDLC = 1, dinamiteMinhocaDLC = 1, flameMinhocaDLC = 20, burningCounter = 0, equipaMinhoca = Just Red, ultimaDirecaoHorizontal = Oeste},MinhocaDLC {posicaoMinhocaDLC = Just (1,3), vidaMinhocaDLC = VivaDLC 100, jetpackMinhocaDLC = 5, escavadoraMinhocaDLC = 5, bazucaMinhocaDLC = 2, minaMinhocaDLC = 1, dinamiteMinhocaDLC = 1, flameMinhocaDLC = 20, burningCounter = 0, equipaMinhoca = Just Blue, ultimaDirecaoHorizontal = Oeste}], armaSelecionada = Nothing, minhocaSelecionada = 0, danosEstado = []}

baseEstado :: EstadoDLC
baseEstado = EstadoDLC {mapaEstadoDLC = [[ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,PedraDLC,PedraDLC,PedraDLC,PedraDLC],[ArDLC,ArDLC,ArDLC,PedraDLC,PedraDLC,PedraDLC,PedraDLC,PedraDLC,ArDLC,ArDLC,ArDLC,ArDLC,ArDLC,PedraDLC,TerraDLC,TerraDLC,TerraDLC,TerraDLC],[ArDLC,PedraDLC,PedraDLC,PedraDLC,TerraDLC,TerraDLC,TerraDLC,TerraDLC,PedraDLC,ArDLC,ArDLC,ArDLC,PedraDLC,TerraDLC,TerraDLC,TerraDLC,TerraDLC,TerraDLC],[ArDLC,PedraDLC,TerraDLC,TerraDLC,PedraDLC,PedraDLC,TerraDLC,TerraDLC,PedraDLC,ArDLC,ArDLC,ArDLC,PedraDLC,TerraDLC,Lava,Lava,TerraDLC,TerraDLC],[PedraDLC,PedraDLC,TerraDLC,PedraDLC,ArDLC,ArDLC,PedraDLC,PedraDLC,TerraDLC,PedraDLC,ArDLC,ArDLC,PedraDLC,TerraDLC,TerraDLC,Lava,TerraDLC,TerraDLC],[PedraDLC,TerraDLC,PedraDLC,AguaDLC,AguaDLC,ArDLC,ArDLC,PedraDLC,TerraDLC,PedraDLC,ArDLC,ArDLC,PedraDLC,TerraDLC,TerraDLC,TerraDLC,TerraDLC,TerraDLC],[PedraDLC,TerraDLC,PedraDLC,AguaDLC,AguaDLC,AguaDLC,AguaDLC,AguaDLC,PedraDLC,AguaDLC,AguaDLC,AguaDLC,PedraDLC,TerraDLC,TerraDLC,PedraDLC,PedraDLC,PedraDLC],[PedraDLC,TerraDLC,TerraDLC,PedraDLC,AguaDLC,AguaDLC,AguaDLC,AguaDLC,AguaDLC,AguaDLC,PedraDLC,PedraDLC,TerraDLC,TerraDLC,TerraDLC,TerraDLC,TerraDLC,TerraDLC],[TerraDLC,TerraDLC,TerraDLC,TerraDLC,PedraDLC,AguaDLC,AguaDLC,AguaDLC,AguaDLC,PedraDLC,TerraDLC,TerraDLC,TerraDLC,TerraDLC,TerraDLC,TerraDLC,TerraDLC,TerraDLC]], objetosEstadoDLC = [], minhocasEstadoDLC = [], armaSelecionada = Nothing, minhocaSelecionada = 0, danosEstado = []}