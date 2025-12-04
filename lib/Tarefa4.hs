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
import Tarefa0_2025 (ePosicaoEstadoLivre, existeMinhoca, existeBarril, verificaVida, encontraQuantidadeArmaMinhoca, ehDisparo, existeBarril, eMinhocaViva)
import Text.ParserCombinators.ReadP (look)
import Data.Maybe (fromMaybe, fromJust)
import Data.List
import Foreign.Marshal (moveArray)

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


-- * ---------------------------------------------------------------------------------

-- * ---------------------------------------------------------------------------------


-- todo sem o sistema de mina ativo
-- | Para um número de ticks desde o início da tática, dado um estado, determina a próxima jogada.
jogadaTatica :: Ticks -> Estado -> (NumMinhoca, Jogada)
jogadaTatica ticks e =
  case minhocasEstado e of
    [] -> defaultBotMove
    minhocas -> 
      case getMinhocasValidasComIndices minhocas of
        [] -> defaultBotMove
        minhocasValidasIdx ->
          if jogadasRestantes > tickingBomb e
          then -- * Jogadas Normais
            let
              i = ticks `mod` length minhocasValidasIdx
              (idxOriginal, minhoca) = minhocasValidasIdx !! i
              pos = case posicaoMinhoca minhoca of Just a -> a
              (onSight, posMira) = minhocaOnSight e pos
            in 
              case (onSight, posMira) of
                (True, Just alvo) -> 
                  if gotAmmo minhoca Bazuca 
                  then (idxOriginal, Dispara Bazuca (getDir8 pos alvo)) 
                  else digFase e idxOriginal minhoca
                _ -> digFase e idxOriginal minhoca
          else -- * Trigger Suicidio
            let
              i = ticks `mod` length minhocasValidasIdx
              (idxOriginal, minhoca) = minhocasValidasIdx !! i
            in 
              forcaSuicidio idxOriginal minhoca e
  where
    jogadasRestantes = 100 - ticks
    mapa = mapaEstado e


    -- | Retorna minhocas válidas e os seus índices originais (i,minhoca)
    getMinhocasValidasComIndices :: [Minhoca] -> [(Int, Minhoca)]
    getMinhocasValidasComIndices minhocas =
      let validasMinhocas = getMinhocasValidas minhocas
      in [(i, m) | (i, m) <- zip [0..] minhocas, m `elem` validasMinhocas]


    forcaSuicidio :: Int -> Minhoca -> Estado -> (NumMinhoca, Jogada)
    forcaSuicidio i minhoca e
      | gotAmmo minhoca Mina = (i, Dispara Mina Norte)
      | gotAmmo minhoca Dinamite = (i, Dispara Dinamite Norte)
      | gotAmmo minhoca Bazuca = (i, Dispara Bazuca Sul)
      | otherwise = suicidioSemArmas i minhoca e


    suicidioSemArmas :: Int -> Minhoca -> Estado -> (NumMinhoca, Jogada)
    suicidioSemArmas i minhoca e =
      let
        mapa = mapaEstado e
        pos = case posicaoMinhoca minhoca of Just a -> a
        posBloco = getPosicoesSquare mapa pos
        blocoAgua = getPosTer mapa Agua posBloco
        blocoTerra = getPosTer mapa Terra posBloco
      in
        case blocoAgua of
          Just a -> (i, Move (getDir8 pos a))
          Nothing -> 
            case blocoTerra of
              Just b -> (i, Dispara Escavadora (getDir8 pos b))
              Nothing -> (i, Move Sul)


    digFase :: Estado -> Int -> Minhoca -> (NumMinhoca, Jogada)
    digFase e i minhoca = 

      let
        mapa = mapaEstado e

        pos = case posicaoMinhoca minhoca of Just a -> a
        posBloco = getPosicoesSquare mapa pos

        blocoTerra = getPosTer mapa Terra posBloco
        posInferior = movePosicao Sul pos
        blocoAr = getPosTerLivre e Ar pos posBloco

      in
        case blocoTerra of
          Just a -> 
            if lastBlockValid mapa a 
            then (i, Dispara Escavadora (getDir8 pos a)) 
            else defaultBotMove
          Nothing -> 
            case getMinhocasValidasComIndices (minhocasEstado e) of 
              [] -> defaultBotMove
              minhocasValidasIdx ->  
                let minhocasValidas = map snd minhocasValidasIdx
                in 
                  if existeMinhoca posInferior minhocasValidas
                  then 
                    case blocoAr of
                      Just a -> (i, Move (getDir8 pos a))
                      Nothing -> defaultBotMove
                  else defaultBotMove




-- Para criar este Bot decidi uma ordem de prioridade para qualquer minhoca que seja controlada
-- O bot primeiro escolhera a minhoca com menos vida com uma posicao valida
-- Assim o objetivo da mesma sera:

-- 1. Existe mina inimiga no mapa? Sim - vai ate ela Nao - Procede
-- 2. O principal é causar danos as minhocas/barris na sua linha de visao usando a bazuca.
-- 3. Caso nao tenha ninguem na sua linha de visao ela deve colocar uma mina perto dela
-- e suicidar se com a dinamite
-- todo implementar sistema de ativacao de mina por minhocas externas 
-- todo implementar sistema de escolha baseado nos ticks restantes -> feito
-- todo possivel suicidio seria tambem correr para as bordas (simples ou com agoritmo)

-- ? Fazer sistema de Escavar tudo a volta -> feito
-- ? Ir para posicao com terra em baixo -> feito




-- * ALGORITMOS DE ESCOLHA DE MINHOCAS

-- Escolhe a Minhoca com menos Vida -> Trabalha apenas com minhocas em posicoes validas
escolherMinhoca :: [Minhoca] -> Int
escolherMinhoca minhocas =
  case getMinhocasValidas minhocas of
    []     -> 0
    (h:t)  -> fst $ foldl escolher (0, h) (zip [1..] t)
  where
    escolher (i1, m1) (i2, m2) =
      if vidaMinhoca m2 < vidaMinhoca m1
        then (i2, m2)
        else (i1, m1)

getMinhocasValidas :: [Minhoca] -> [Minhoca]
getMinhocasValidas [] = []
getMinhocasValidas (h:t) =
  if posicaoMinhoca h /= Nothing && eMinhocaViva h
    then h : getMinhocasValidas t
    else getMinhocasValidas t


minhocaOnSight :: Estado -> Posicao -> (Bool, Maybe Posicao)
minhocaOnSight e pos =
    let mapa          = mapaEstado e
        posicoesLV    = getPosicoesBazuca mapa pos
        minhocasVivas = filter verificaVida (minhocasEstado e)

        barris = getBarris (objetosEstado e)

        minhocasInSight = filter (`existeMinhoca` minhocasVivas) posicoesLV
        barrisInSight = filter (`existeBarril` barris) posicoesLV


        maisPerto = escolheMaisPerto pos (minhocasInSight ++ barrisInSight)

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


    in (not (null minhocasInSight && null barrisInSight), maisPerto)






-- | Verifica se a posição inferior ao bloco na posição está fora da Matriz, ou seja se é valido ou nao
lastBlockValid :: Mapa -> Posicao -> Bool
lastBlockValid mapa pos = let posInferior = movePosicao Sul pos in ePosicaoMatrizValida posInferior mapa

getDimensoesMatriz :: Mapa -> (Int, Int)
getDimensoesMatriz m = (length m, length (head m))  -- (dY, dX)



-- [Norte, Sul, Este, Oeste, Nordeste, Noroeste, Sudeste, Sudoeste] -> Possiveis direcoes

getPosicoesBazuca :: Mapa -> Posicao -> [Posicao]
getPosicoesBazuca mapa pos = sort (concatMap (posicoesDirecao mapa pos) direcoes)
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

getPosicoesSquare :: Mapa -> Posicao -> [Posicao]
getPosicoesSquare mapa pos = sort (concatMap (posicoesDirecao mapa pos) direcoes)
    where
        direcoes :: [Direcao]
        direcoes =
          [Norte, Sul, Este, Oeste, Nordeste, Noroeste, Sudeste, Sudoeste]

        posicoesDirecao :: Mapa -> Posicao -> Direcao -> [Posicao]
        posicoesDirecao m p dir =
          let (dY, dX) = getDimensoesMatriz m
              (y, x)   = movePosicao dir p
          in if y < 0 || x < 0 || y >= dY || x >= dX
                then []
                else [(y, x)]




-- Verifica se existe terreno x numa posicao x e devolve a primeira ocorrencia deste elemento
getPosTer :: Mapa -> Terreno -> [Posicao] -> Maybe Posicao
getPosTer _ _ [] = Nothing
getPosTer mapa tipo (h:t) = case encontraPosicaoMatriz h mapa of
  Just a -> if tipo == a then Just h else getPosTer mapa tipo t
  Nothing -> Nothing

temMinaPos :: Posicao -> Mapa -> [Objeto] -> (Bool, Maybe Posicao)
temMinaPos pos mapa objs = case minas of 
  [] -> (False, Nothing)
  a -> checkMina (head a) posMina

  where
    minas = filter ehMina objs
    posMina = getPosicoesSquare mapa pos

    

    ehMina :: Objeto -> Bool
    ehMina d@(Disparo _ _ arma _ _) = ehDisparo d && (case arma of
        Mina ->  True
        _ -> False)
    ehMina d@(Barril _ _) = False

    checkMina :: Objeto -> [Posicao] -> (Bool, Maybe Posicao)
    checkMina _ [] = (False, Nothing)
    checkMina d@(Disparo posM _ _ _ _) (h:t) = if posM == h then (True, Just posM) else checkMina d t

getPosTerLivre :: Estado -> Terreno -> Posicao -> [Posicao] -> Maybe Posicao
getPosTerLivre _ _ _ [] = Nothing
getPosTerLivre e tipo p ((y,x):t) = case encontraPosicaoMatriz (y,x) mapa of
    Just a -> if tipo == a && (y,x) /= (movePosicao Norte p) && ePosicaoEstadoLivre (y,x) e then Just (y,x) else getPosTerLivre e tipo p t
    Nothing -> Nothing
  where
    mapa = mapaEstado e  

objs = [Disparo {posicaoDisparo = (0,1), direcaoDisparo = Norte, tipoDisparo = Dinamite, tempoDisparo = Nothing, donoDisparo = 0}, Disparo {posicaoDisparo = (2,5), direcaoDisparo = Oeste, tipoDisparo = Bazuca, tempoDisparo = Nothing, donoDisparo = 0}]

-- * PROCESSAMENTO DE SUICIDIO

-- FUncao que devolve o tempo necessario para que todas as minhocas se suicidem no estado
-- Este tempo é calculado de algumas formas
-- Tempo para dinamite explodir - se houver dinamite  (colocar -1 tick, explodir - 4)
-- Tempo para bazuca explodir - se tiver balas (colocar 1- tick, explodir - 1)
-- Tempo para colocar mina - se tiver mina
-- Tempo para registrar explosao - 1 tick


tickingBomb :: Estado -> Ticks
tickingBomb e = let

  minhocas = minhocasEstado e
  (numTerr, terroristas) = getTerroristas minhocas
  armas = getArmasSuicidio terroristas

  tempoArmas = getGunTimeCount (nub armas)

  threshold = 10 + length minhocas -- Valor default para segurança

  getTerroristas :: [Minhoca] -> (Int, [Minhoca])
  getTerroristas [] = (0, [])
  getTerroristas (h:t) =
      let (cont, lista) = getTerroristas t
          (_, podeSuicidar) = canSuicide h
      in if podeSuicidar
        then (cont + 1, h : lista)
        else (cont, lista)

  -- Devolve as armas usadas para suicidio
  getArmasSuicidio :: [Minhoca] -> [TipoArma]
  getArmasSuicidio [] = []
  getArmasSuicidio (h:t) = let (arma, _) = canSuicide h
                          in fromJust arma : getArmasSuicidio t

  -- Devolve o tempo das armas para explodirem
  getGunTimeCount :: [TipoArma] -> Int
  getGunTimeCount [] = 0
  getGunTimeCount (h:t) = case h of
    Bazuca -> 1 + getGunTimeCount t
    Dinamite ->  5 + getGunTimeCount t




  in tempoArmas + numTerr + threshold

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





-- * FUNCOES AUXILIARES


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








































mapa1 = [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Ar,Pedra,Agua,Agua,Agua,Agua,Agua]
        ,[Terra,Terra,Terra,Terra,Pedra,Pedra,Agua,Agua,Agua,Agua]
        ,[Terra,Terra,Terra,Terra,Terra,Pedra,Pedra,Pedra,Agua,Agua]
        ]




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
        ,Minhoca {posicaoMinhoca = Just (1,4), vidaMinhoca = Viva 50, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}
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

