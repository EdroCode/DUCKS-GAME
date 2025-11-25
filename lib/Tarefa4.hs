{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-|
Module      : Tarefa4
Description : Implementar uma tática de jogo.

Módulo para a realização da Tarefa 4 de LI1\/LP1 em 2025\/26.
-}
module Tarefa4 where

import Data.Either

import Labs2025
import Tarefa2
import Tarefa3
import Tarefa0_2025 (ePosicaoEstadoLivre, existeMinhoca)

-- | Função principal da Tarefa 4. Dado um estado retorna uma lista de jogadas, com exatamente 100 jogadas.
tatica :: Estado -> [(NumMinhoca,Jogada)]
tatica e = reverse $ snd $ foldl avancaTatica (e,[]) [0..99]

-- | Aplica uma sequência de jogadas a um estado, avançando o tempo entre jogadas.
avancaTatica :: (Estado,[(NumMinhoca,Jogada)]) -> Ticks -> (Estado,[(NumMinhoca,Jogada)])
avancaTatica (e,js) tick = (avancaJogada j e,j:js)
    where j = jogadaTatica tick e

-- | Aplica uma jogada de uma minhoca a um estado, e avança o tempo.
avancaJogada :: (NumMinhoca,Jogada) -> Estado -> Estado
avancaJogada (i,j) e@(Estado _ objetos minhocas) = foldr aplicaDanos e'' danoss''
    where
    e'@(Estado mapa' objetos' minhocas') = efetuaJogada i j e
    minhocas'' = map (avancaMinhocaJogada e') (zip3 [0..] minhocas minhocas')
    (objetos'',danoss'') = partitionEithers $ map (avancaObjetoJogada (e' { minhocasEstado = minhocas''}) objetos) (zip [0..] objetos')
    e'' = Estado mapa' objetos'' minhocas''

-- | Avança o tempo para o estado de uma minhoca, se não efetuou a última jogada.
avancaMinhocaJogada :: Estado -> (NumMinhoca,Minhoca,Minhoca) -> Minhoca
avancaMinhocaJogada e (i,minhoca,minhoca') = if posicaoMinhoca minhoca == posicaoMinhoca minhoca'
    then avancaMinhoca e i minhoca'
    else minhoca'

-- | Avança o tempo para o estado de um objeto, se não foi criado pela última jogada.
avancaObjetoJogada :: Estado -> [Objeto] -> (NumObjeto,Objeto) -> Either Objeto Danos
avancaObjetoJogada e objetos (i,objeto') = if elem objeto' objetos
    then avancaObjeto e i objeto'
    else Left objeto'

-- | Para um número de ticks desde o início da tática, dado um estado, determina a próxima jogada.
jogadaTatica :: Ticks -> Estado -> (NumMinhoca,Jogada)
jogadaTatica _ e = let minhoca = head (minhocasEstado e)
                       
                       pos = case posicaoMinhoca minhoca of Just a -> a
                       newDir = moveCheck pos Este e 
                   in
                    (0, Move newDir)

-- tatica possivel, suicidio multipli com terrenos


moveCheck :: Posicao -> Direcao -> Estado -> Direcao
moveCheck pos dir e = if posValida then dir else (case getXWay dir of
    Este -> direcaoOposta (getXWay dir)
    Oeste -> direcaoOposta (getXWay dir)
    _ -> dir)
                    where
                        
                        novaPos = movePosicao dir pos
                        posValida = ePosicaoEstadoLivre novaPos e

                        getXWay :: Direcao -> Direcao
                        getXWay d = case d of
                            Noroeste  -> Oeste
                            Sudoeste  -> Oeste
                            Nordeste  -> Este
                            Sudeste   -> Este
                            _         -> d

ola :: Estado
ola = Estado
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
        [Minhoca {posicaoMinhoca = Just (2,1), vidaMinhoca = Viva 70, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}
        ,Minhoca {posicaoMinhoca = Just (2,4), vidaMinhoca = Viva 50, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}
        ]
    }

areaDetect :: Int -> Posicao -> Estado -> Bool
areaDetect range pos e =  all (`ePosicaoEstadoLivre` e) (getArea range pos) 

getArea :: Int -> (Int, Int) -> [(Int, Int)]
getArea d (x, y) = geraLinha (x - d)
  where
    geraLinha i =
      if i > x + d
         then []
         else geraColuna i (y - d) ++ geraLinha (i + 1)

    geraColuna i j
      | j > y + d = []
      | (i, j) == (x, y) = geraColuna i (j + 1)
      | otherwise = (i, j) : geraColuna i (j + 1)




