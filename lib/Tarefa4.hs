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
import Tarefa0_2025 (ePosicaoEstadoLivre, existeMinhoca, verificaVida, encontraQuantidadeArmaMinhoca)
import Text.ParserCombinators.ReadP (look)

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


m1 = [[Ar,Ar,Ar,Pedra,Ar,Ar,Terra,Ar,Ar,Pedra,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Pedra,Ar,Terra,Ar,Pedra,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Pedra,Terra,Pedra,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Ar,Terra,Terra,Terra,Terra,Terra,Terra,Terra]
        ,[Ar,Ar,Ar,Ar,Ar,Pedra,Terra,Pedra,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Pedra,Ar,Terra,Ar,Pedra,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Pedra,Ar,Ar,Terra,Ar,Ar,Pedra,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Pedra,Ar,Ar,Ar,Terra,Ar,Ar,Ar,Pedra,Ar,Ar,Ar]
        ]

e2 = Estado
    { mapaEstado =
        [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra]
        ]
    , objetosEstado =
        []
    , minhocasEstado =
        [Minhoca {posicaoMinhoca = Just (4,1), vidaMinhoca = Viva 30, jetpackMinhoca = 1, escavadoraMinhoca = 0, bazucaMinhoca = 0, minaMinhoca = 1, dinamiteMinhoca = 0}
        ,Minhoca {posicaoMinhoca = Just (4,9), vidaMinhoca = Viva 100, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}
        ]
    }






-- | Para um número de ticks desde o início da tática, dado um estado, determina a próxima jogada.
jogadaTatica :: Ticks -> Estado -> (NumMinhoca,Jogada)
jogadaTatica _ e = case posicaoMinhoca minhocaAtual of
        Just a -> if onSight && gotAmmo minhocaAtual Bazuca
            then case posMira of
                Just b -> (i,Dispara Bazuca (getDir a b))
                Nothing -> (i, Move Sul) -- temporario

            else if suicida then (i,commitSuicide t) else (i, Move Sul)
            where
                (onSight, posMira) = minhocaOnSight e a
                (t, suicida) = canSuicide minhocaAtual 
        Nothing -> (i,Move Sul)
    where
        minhocas = minhocasEstado e
        minhocaAtual = head minhocas
        restantes = tail minhocas
        i = 0 -- mudar depois

        

-- mudar no docker
-- mixconfig






-- tatica possivel, suicidio multipli com terrenos
-- BRUTEFORFCE


-- lookForNxtLiv :: Posicao -> Posicao -> Posicao 

-- Move a minhoca ate um raio de 1 bloco a volta da minhoca mais proxima. Devolve a posicao seguinte no Path




-- Verifica se na linha de visao da bazuca existe uma minhoca
minhocaOnSight :: Estado -> Posicao -> (Bool, Maybe Posicao)
minhocaOnSight e pos =
    let mapa          = mapaEstado e
        posicoesLV    = getPosicoesBazuca mapa pos
        minhocasVivas = filter verificaVida (minhocasEstado e)

        minhocasInSight = filter (`existeMinhoca` minhocasVivas) posicoesLV

        maisPerto = escolheMaisPerto pos minhocasInSight

        -- devolve a posição da minhoca mais proxima
        escolheMaisPerto :: Posicao -> [Posicao] -> Maybe Posicao
        escolheMaisPerto _ []     = Nothing
        escolheMaisPerto p (x:xs) = Just (foldl (comparaDist p) x xs)

        -- escolhe a posicao mais proxima de uma posicao entre 2 posicoes
        comparaDist :: Posicao -> Posicao -> Posicao -> Posicao
        comparaDist p melhor atual =
            if distAtoB p atual < distAtoB p melhor then atual else melhor

    in (not (null minhocasInSight), maisPerto)


getDimensoesMatriz :: Mapa -> (Int, Int)
getDimensoesMatriz m = (length (head m), length m)


-- Junta todas as posições
getPosicoesBazuca :: Mapa -> Posicao -> [Posicao]
getPosicoesBazuca mapa pos = concatMap (posicoesDirecao mapa pos) direcoes
    where
        -- Todas as direções
        direcoes :: [Direcao]
        direcoes = [Norte, Sul, Este, Oeste,
                    Nordeste, Noroeste, Sudeste, Sudoeste]

        --Devolve uma lista de posições dentro da matriz nas 8 direções
        posicoesDirecao :: Mapa -> Posicao -> Direcao -> [Posicao]
        posicoesDirecao m p dir =
            let (dX, dY) = getDimensoesMatriz m
                anda (x,y) =
                    if x < 0 || y < 0 || x >= dX || y >= dY
                    then []
                    else (x,y) : anda (movePosicao dir (x,y))
            in anda (movePosicao dir p)

-- Verifica se a mionhoca tem municao de uma certa arma
gotAmmo :: Minhoca -> TipoArma -> Bool
gotAmmo minh tipo = encontraQuantidadeArmaMinhoca tipo minh > 0



-- Assumimos que temos somente estados válidos logo não teremos sobreposição de minhocas
-- Devolve a direção que uma minhoca está em relação a outra
-- (não funciona fora dos 8 eixos mas devolve na mesma o mais próximo)
getDir :: Posicao -> Posicao -> Direcao
getDir (y,x) (b,a)
  | y == b = if a < x then Oeste else Este
  | x == a = if b < y then Norte else Sul
  | b < y && a < x = Noroeste
  | b < y && a > x = Nordeste
  | b > y && a < x = Sudoeste
  | otherwise = Sudeste


distAtoB :: Posicao -> Posicao -> Double
distAtoB (x,y) (a,b) = sqrt ((dx * dx) + (dy * dy))
  where
    dx = fromIntegral (x - a)
    dy = fromIntegral (y - b)


-- Ordem prioridade para o suicidio
-- Dinamite
-- Bazuca

canSuicide :: Minhoca -> (Maybe TipoArma,Bool)
canSuicide m
  | dinamiteMinhoca m > 0 = (Just Dinamite, True)
  | bazucaMinhoca m > 0 = (Just Bazuca,True)
  | otherwise = (Nothing, False)


commitSuicide :: Maybe TipoArma -> Jogada
commitSuicide tipo = case tipo of 
    Just Dinamite -> Dispara Dinamite Norte
    Just Bazuca -> Dispara Bazuca Sul




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




