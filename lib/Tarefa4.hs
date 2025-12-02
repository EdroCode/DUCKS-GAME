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
import Tarefa0_2025 (ePosicaoEstadoLivre, existeMinhoca, existeBarril, verificaVida, encontraQuantidadeArmaMinhoca, ehDisparo, existeBarril)
import Text.ParserCombinators.ReadP (look)
import Data.Maybe (fromMaybe)
import Data.List

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




-- todo sem o sistema de mina ativo
-- | Para um número de ticks desde o início da tática, dado um estado, determina a próxima jogada.
jogadaTatica :: Ticks -> Estado -> (NumMinhoca, Jogada)
jogadaTatica ticks e =
  case minhocasEstado e of
    [] -> (100, Move Sul)  -- ! falar com stor 
    minhocas ->
      let (i, minhoca) = escolherMinhoca minhocas
          pos = case posicaoMinhoca minhoca of Just a -> a
          (onSight, posMira) = minhocaOnSight e pos
          jogadasRestantes   = 100 - ticks
          
          
      in case (onSight, posMira) of
           (True, Just alvo) -> if gotAmmo minhoca Bazuca then (i, Dispara Bazuca (getDir8 pos alvo)) else suicidioFase jogadasRestantes i minhoca
           _ -> suicidioFase jogadasRestantes i minhoca -- todo suicidio

-- nome temporario representa o processo de suicidio
suicidioFase :: Ticks -> Int -> Minhoca -> (NumMinhoca, Jogada)
suicidioFase ticksRestantes i minhoca = 
                if ticksRestantes < 10  -- todo depois mudar o 5
                  then if gotAmmo minhoca Mina
                    then (i, Dispara Mina Norte)
                    else if gotAmmo minhoca Dinamite 
                          then (i, Dispara Dinamite Norte)
                          else if gotAmmo minhoca Bazuca
                            then (i, Dispara Bazuca Sul)
                            else (i, Move Sul)
                  else (i, Move Sul)  -- todo Nao faz  -> movimento depois      

-- Para criar este Bot decidi uma ordem de prioridade para qualquer minhoca que seja controlada
-- O bot primeiro escolhera a minhoca com menos vida com uma posicao valida
-- Assim o objetivo da mesma sera:

-- 1. Existe mina inimiga no mapa? Sim - vai ate ela Nao - Procede
-- 2. O principal é causar danos as minhocas/barris na sua linha de visao usando a bazuca.
-- 3. Caso nao tenha ninguem na sua linha de visao ela deve colocar uma mina perto dela
-- e suicidar se com a dinamite
-- todo implementar sistema de ativacao de mina por minhocas externas
-- todo implementar sistema de escolha baseado nos ticks restantes
-- todo possivel suicidio seria tambem correr para as bordas (simples ou com agoritmo)




-- Escolhe a Minhoca com menos Vida -> Trabalha apenas com minhocas em posicoes validas
escolherMinhoca :: [Minhoca] -> (Int, Minhoca)
escolherMinhoca minhocas =
  let (h:t) = getMinhocasValidas minhocas
  in foldl escolher (0, h) (zip [1..] t)
  where
    escolher (i1, m1) (i2, m2) =
      if vidaMinhoca m2 < vidaMinhoca m1
         then (i2, m2)
         else (i1, m1)

getMinhocasValidas :: [Minhoca] -> [Minhoca]
getMinhocasValidas [] = []
getMinhocasValidas (h:t) =
    if posicaoMinhoca h /= Nothing
       then h : getMinhocasValidas t
       else getMinhocasValidas t


escolherAtaque:: Minhoca -> Posicao -> Posicao -> Jogada
escolherAtaque mh posAtual posMira = undefined
--    | gotAmmo mh Bazuca -> Dispara Bazuca (getDir8 posAtual alvo)
--      | gotAmmo mh Mina   -> Dispara Mina  (getDir8 posAtual alvo)
 --     | otherwise         -> Move Sul


-- tatica possivel, suicidio multipli com terrenos
-- BRUTEFORFCE


-- lookForNxtLiv :: Posicao -> Posicao -> Posicao 

-- Move a minhoca ate um raio de 1 bloco a volta da minhoca mais proxima. Devolve a posicao seguinte no Path

-- Verifica se na linha de visao da (tipo de arma) existe uma minhoca

acaba = Estado { mapaEstado =
        [[Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar]]
    , objetosEstado =
        []
    , minhocasEstado =
        [Minhoca {posicaoMinhoca = Just (1,1), vidaMinhoca = Viva 30, jetpackMinhoca = 1, escavadoraMinhoca = 0, bazucaMinhoca = 0, minaMinhoca = 1, dinamiteMinhoca = 0}
        ,Minhoca {posicaoMinhoca = Just (1,3), vidaMinhoca = Viva 100, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}
        ]
    }

minhocaOnSight :: Estado -> Posicao -> (Bool, Maybe Posicao)
minhocaOnSight e pos =
    let mapa          = mapaEstado e
        posicoesLV    = getPosicoesBazuca mapa pos
        minhocasVivas = filter verificaVida (minhocasEstado e)

        barris = getBarris (objetosEstado e)

        minhocasInSight = filter (`existeMinhoca` minhocasVivas) posicoesLV
        barrisInSight = filter (`existeBarril` barris) posicoesLV


        maisPerto = escolheMaisPerto pos minhocasInSight

        -- devolve a posição da minhoca mais proxima
        escolheMaisPerto :: Posicao -> [Posicao] -> Maybe Posicao
        escolheMaisPerto _ []     = Nothing
        escolheMaisPerto p (x:xs) = Just (foldl (comparaDist p) x xs)

        -- escolhe a posicao mais proxima de uma posicao entre 2 posicoes
        comparaDist :: Posicao -> Posicao -> Posicao -> Posicao
        comparaDist p melhor atual =
            if distAtoB p atual < distAtoB p melhor then atual else melhor
        
        -- retorna uma lista dos barris presentes no mapa
        getBarris :: [Objeto] -> [Objeto]
        getBarris [] = []
        getBarris (h:t) = if ehDisparo h then getBarris t else h : getBarris t

    in (not (null minhocasInSight), maisPerto)





getDimensoesMatriz :: Mapa -> (Int, Int)
getDimensoesMatriz m = (length m, length (head m))  -- (dY, dX)



-- [Norte, Sul, Este, Oeste, Nordeste, Noroeste, Sudeste, Sudoeste] -> Possiveis direcoes
-- Esta funcao é uma alternativa a amnterior, lea é mais versatil pois é possivel escolher o range e as direcoes
-- Caso o range seja Nothing ou 0, usamos o maior range -> As bordas do mapa
-- (Porque nao usar somente um int? Porque usando um maybe temos mais readability)
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
          let (dY, dX) = getDimensoesMatriz m
              anda (y,x) =
                if y < 0 || x < 0 || y >= dY || x >= dX
                  then []
                  else (y,x) : anda (movePosicao dir (y,x))
          in anda (movePosicao dir p)


-- (dY, dX)


-- Verifica se a mionhoca tem municao de uma certa arma
gotAmmo :: Minhoca -> TipoArma -> Bool
gotAmmo minh tipo = encontraQuantidadeArmaMinhoca tipo minh > 0



-- Assumimos que temos somente estados válidos logo não teremos sobreposição de minhocas
-- Devolve a direção que uma minhoca está em relação a outra
-- (não funciona fora dos 8 eixos mas devolve na mesma o mais próximo)
getDir8 :: Posicao -> Posicao -> Direcao
getDir8 (y,x) (b,a)
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
        [Minhoca {posicaoMinhoca = Just (2,1), vidaMinhoca = Viva 70, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 0}
        ,Minhoca {posicaoMinhoca = Just (1,4), vidaMinhoca = Viva 50, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 0}
        ]
    }


map1 = [[Ar,Ar,Ar,Pedra,Ar,Ar,Terra,Ar,Ar,Pedra,Ar,Ar,Ar,Ar]
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

