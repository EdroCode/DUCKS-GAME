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
import Tarefa0_2025 (ePosicaoEstadoLivre, existeMinhoca, existeBarril, verificaVida, encontraQuantidadeArmaMinhoca, ehDisparo, existeBarril, eMinhocaViva, ePosicaoMapaLivre)
import Text.ParserCombinators.ReadP (look)
import Data.Maybe (fromMaybe, fromJust)
import Data.List
import Foreign.Marshal (moveArray)
import Data.Semigroup (Min(getMin))

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


-- todo sem o sistema de mina ativodefaultBotMove  :: (NumMinhoca, Jogada)

-- | Para um número de ticks desde o início da tática, dado um estado, determina a próxima jogada.
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
              numMinhocasValidas = length minhocasValidasIdx
              i = ticks `mod` numMinhocasValidas
              (idOriginal, minhoca) = minhocasValidasIdx !! i
              pos = case posicaoMinhoca minhoca of Just a -> a
              (onSight, posMira) = minhocaOnSight e pos
            in
              case (onSight, posMira) of
                (True, Just alvo) ->
                  if gotAmmo minhoca Bazuca
                  then (idOriginal, Dispara Bazuca (getDir8 pos alvo))
                  else digFase idOriginal minhoca
                _ -> digFase idOriginal minhoca
          else -- * Trigger Suicidio
            let
              numMinhocasValidas = length minhocasValidasIdx
              i = ticks `mod` numMinhocasValidas
              (idOriginal, minhoca) = minhocasValidasIdx !! i
            in
              forcaSuicidio idOriginal minhoca
  where
    jogadasRestantes = 100 - ticks
    mapa = mapaEstado e


    -- | Retorna minhocas válidas e os seus índices originais (i,minhoca)
    getMinhocasValidasComIndices :: [Minhoca] -> [(Int, Minhoca)]
    getMinhocasValidasComIndices minhocas =
      let validasMinhocas = getMinhocasValidas minhocas
      in [(i, m) | (i, m) <- zip [0..] minhocas, m `elem` validasMinhocas]


    forcaSuicidio :: Int -> Minhoca -> (NumMinhoca, Jogada)
    forcaSuicidio i minhoca
      | gotAmmo minhoca Mina = (i, Dispara Mina Norte)
      | gotAmmo minhoca Dinamite = (i, Dispara Dinamite Norte)
      | gotAmmo minhoca Bazuca = (i, Dispara Bazuca Sul)
      | otherwise = suicidioSemArmas i minhoca


    suicidioSemArmas :: Int -> Minhoca -> (NumMinhoca, Jogada)
    suicidioSemArmas i minhoca =
      let
        pos = case posicaoMinhoca minhoca of Just a -> a
        posBloco = getPosicoesSquare mapa pos
        blocoAgua = getPosTer mapa Agua posBloco
        blocoTerra = getPosTer mapa Terra posBloco

        posBlocoInv = getPosicoesSquareInv pos
        blocoNegro = getPosInv mapa posBlocoInv

        (podeSaltar, dirSalto) = canBungeJump mapa pos
        (voidDisponivel, dirVoid) = canFollowVoid mapa pos -- Void disponivel - tem uma posicao vazia perto

      in
        case blocoAgua of
          Just a -> (i, Move (getDir8 pos a))
          Nothing ->
            case blocoTerra of
              Just b -> (if not (ePosicaoMatrizValida (movePosicao Sul b) mapa) && gotAmmo minhoca Escavadora then (i, Dispara Escavadora (getDir8 pos b)) else (case blocoNegro of

                  Just posInvalida -> (i, Move (getDir8 pos posInvalida))
                  Nothing -> case (podeSaltar, dirSalto) of
                    (True, Just d) -> (i, Move d)
                    _ -> complexBotMove e (i, minhoca)))

              Nothing -> case blocoNegro of

                Just posInvalida -> (i, Move (getDir8 pos posInvalida))
                Nothing -> case (podeSaltar, dirSalto) of
                  (True, Just d) -> (i, Move d)
                  _ -> complexBotMove e (i, minhoca)







    digFase :: Int -> Minhoca -> (NumMinhoca, Jogada)
    digFase i minhoca =

      let

        pos = case posicaoMinhoca minhoca of Just a -> a
        posBloco = getPosicoesSquare mapa pos

        blocoTerra = getPosTer mapa Terra posBloco
        posInferior = movePosicao Sul pos
        blocoAr = getPosTer mapa Ar posBloco

      in
        if gotAmmo minhoca Escavadora
          then case blocoTerra of
            Just a ->
              if lastBlockValid mapa a
              then (i, Dispara Escavadora (getDir8 pos a))
              else complexBotMove e (i, minhoca)
            Nothing ->
              case getMinhocasValidasComIndices (minhocasEstado e) of
                [] -> defaultBotMove
                minhocasValidasIdx ->
                  let minhocasValidas = map snd minhocasValidasIdx
                  in
                    if existeMinhoca posInferior minhocasValidas
                    then
                      case blocoAr of -- Caso tenha uma minhoca em baixo dela ela sai 
                        Just a -> (i, Move (getDir8 pos a)) -- !
                        Nothing -> defaultBotMove
                    else complexBotMove e (i, minhoca)
          else
              case getMinhocasValidasComIndices (minhocasEstado e) of
                [] -> defaultBotMove
                minhocasValidasIdx ->
                  let minhocasValidas = map snd minhocasValidasIdx
                  in
                    if existeMinhoca posInferior minhocasValidas
                    then
                      case blocoAr of -- Caso tenha uma minhoca em baixo dela ela sai 
                        Just a -> (i, Move (getDir8 pos a)) -- !
                        Nothing -> defaultBotMove
                    else complexBotMove e (i, minhoca)




-- | Algoritmo alternativo ao defaultBotMove
complexBotMove :: Estado -> (Int, Minhoca) -> (NumMinhoca, Jogada)
complexBotMove e (i, minhoca) = case minhocaProx of
                                  Just m -> let posM = case posicaoMinhoca m of Just a -> a
                                                dirMinhoca = getDir8 pos posM
                                            in (i, Move (getXWay dirMinhoca))

                                  Nothing -> (i, Move Sul)



  where
    pos = case posicaoMinhoca minhoca of Just a -> a
    minhocasValidas = getMinhocasValidas (minhocasEstado e)
    minhocaProx = getMinhProx minhocasValidas pos
    

    getMinhProx :: [Minhoca] -> Posicao -> Maybe Minhoca
    getMinhProx [] _ = Nothing
    getMinhProx [m1] _ = Just m1
    getMinhProx (m1:m2:t) p = 
      let p1 = case posicaoMinhoca m1 of Just a -> a
          p2 = case posicaoMinhoca m2 of Just a -> a

      in if distAtoB p p1 > distAtoB p p2 
        then getMinhProx (m2:t) p
        else getMinhProx (m1:t) p


-- todo usar imagens para compor texto




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

-- ! Todo uma boa logica de move default seria seguir na linha das minhocas proximas, mas isso requer
-- ! polir como trabalhamos os indices que parece ser o maior problema atualmente

-- ? Fazer sistema de Escavar tudo a volta -> feito
-- ? Ir para posicao com terra em baixo -> feito


-- * ---------------------------------------------------------------------------------
-- * ALGORITMOS DE ESCOLHA DE MINHOCAS

-- | Escolhe a minhoca com menos vida
-- | Trabalha apenas com minhocas em posições válidas
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

-- | Recebe uma lista e filtra as minhocas válidas (com posição e vivas)
getMinhocasValidas :: [Minhoca] -> [Minhoca]
getMinhocasValidas [] = []
getMinhocasValidas (h:t) =
  if posicaoMinhoca h /= Nothing && eMinhocaViva h
    then h : getMinhocasValidas t
    else getMinhocasValidas t


-- * DETEÇÃO DE ALVOS

-- | Verifica se há minhocas ou barris na linha de visão
-- Devolve (True, Just Posicao) se houver alvo, (False, Nothing) caso contrário

minhocaOnSight :: Estado -> Posicao -> (Bool, Maybe Posicao)
minhocaOnSight e pos =

    let mapa          = mapaEstado e
        posicoesLV    = getPosicoesBazuca mapa pos
        minhocasVivas = filter verificaVida (minhocasEstado e)

        barris = getBarris (objetosEstado e)

        minhocasInSight = filter (`existeMinhoca` minhocasVivas) posicoesLV
        barrisInSight = filter (`existeBarril` barris) posicoesLV

        maisPerto = escolheMaisPerto pos (minhocasInSight ++ barrisInSight)

        -- | Devolve a posição do alvo mais próximo
        escolheMaisPerto :: Posicao -> [Posicao] -> Maybe Posicao
        escolheMaisPerto _ []     = Nothing
        escolheMaisPerto p (x:xs) = Just (foldl (comparaDist p) x xs)

        -- | Escolhe a posição mais próxima entre duas posições
        comparaDist :: Posicao -> Posicao -> Posicao -> Posicao
        comparaDist p melhor atual =
            if distAtoB p atual < distAtoB p melhor then atual else melhor

        -- | Retorna uma lista dos barris presentes no mapa
        getBarris :: [Objeto] -> [Objeto]
        getBarris [] = []
        getBarris (h:t) = if ehDisparo h then getBarris t else h : getBarris t

    in (not (null minhocasInSight && null barrisInSight), maisPerto)


-- * VALIDAÇÃO E DIMENSÕES

-- | Verifica se a posição inferior ao bloco está dentro da matriz
lastBlockValid :: Mapa -> Posicao -> Bool
lastBlockValid mapa pos = let posInferior = movePosicao Sul pos in ePosicaoMatrizValida posInferior mapa

-- | Devolve as dimensões da matriz (altura, largura)
getDimensoesMatriz :: Mapa -> (Int, Int)
getDimensoesMatriz m = (length m, length (head m))  -- (dY, dX)


-- * CÁLCULO DE POSIÇÕES

-- | Devolve todas as posições visíveis nas 8 direções até ao limite da matriz
getPosicoesBazuca :: Mapa -> Posicao -> [Posicao]
getPosicoesBazuca mapa pos = sort (concatMap (posicoesDirecao mapa pos) direcoes)
    where
        -- Todas as direções
        direcoes :: [Direcao]
        direcoes = [Norte, Sul, Este, Oeste,
                    Nordeste, Noroeste, Sudeste, Sudoeste]

        -- | Devolve uma lista de posições dentro da matriz numa direção
        posicoesDirecao :: Mapa -> Posicao -> Direcao -> [Posicao]
        posicoesDirecao m p dir =
          let (dY, dX) = getDimensoesMatriz m
              anda (y,x) =
                if y < 0 || x < 0 || y >= dY || x >= dX
                  then []
                  else (y,x) : anda (movePosicao dir (y,x))
          in anda (movePosicao dir p)

-- | Devolve as posições validas adjacentes (quadrado 3x3) a uma posição
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

-- | Devolve todas as posições adjacentes (quadrado 3x3) a uma posição
-- | Inclui tanto as posições válidas como as inválidas (fora da matriz)
getPosicoesSquareInv :: Posicao -> [Posicao]
getPosicoesSquareInv pos = sort (concatMap (posicoesDirecao pos) direcoes)
  where
    direcoes :: [Direcao]
    direcoes = [Norte, Sul, Este, Oeste, Nordeste, Noroeste, Sudeste, Sudoeste]

    posicoesDirecao :: Posicao -> Direcao -> [Posicao]
    posicoesDirecao p dir =
      let (y, x) = movePosicao dir p
      in [(y, x)]  -- Devolve sempre a posição, válida ou não

-- * PROCURA DE TERRENO

-- | Verifica se existe terreno de um tipo numa lista de posições
-- Devolve a primeira ocorrência encontrada
getPosTer :: Mapa -> Terreno -> [Posicao] -> Maybe Posicao
getPosTer _ _ [] = Nothing
getPosTer mapa tipo (h:t) = case encontraPosicaoMatriz h mapa of
  Just a -> if tipo == a then Just h else getPosTer mapa tipo t
  Nothing -> Nothing

-- | Verifica se existe uma posição inválida numa lista de posições
-- Devolve a primeira ocorrência encontrada
getPosInv :: Mapa -> [Posicao] -> Maybe Posicao
getPosInv _  [] = Nothing
getPosInv mapa (h:t) = if ePosicaoMatrizValida h mapa then getPosInv mapa t else Just h

-- | Verifica se existe terreno de um tipo numa lista de posições
-- Devolve a primeira ocorrência encontrada
getPosLivre:: Mapa -> [Posicao] -> Maybe Posicao
getPosLivre _ [] = Nothing
getPosLivre mapa (h:t) = case encontraPosicaoMatriz h mapa of
  Just a -> if a == Ar || a == Agua then Just h else getPosLivre mapa t
  Nothing -> Nothing


-- | Verifica se existe uma mina adjacente a uma posição
-- Devolve (True, Just Posicao) se houver mina, (False, Nothing) caso contrário
temMinaPos :: Posicao -> Mapa -> [Objeto] -> (Bool, Maybe Posicao)
temMinaPos pos mapa objs = case minas of
  [] -> (False, Nothing)
  a -> checkMina (head a) posMina

  where
    minas = filter ehMina objs
    posMina = getPosicoesSquare mapa pos

    -- | Verifica se um objeto é uma mina
    ehMina :: Objeto -> Bool
    ehMina d@(Disparo _ _ arma _ _) = ehDisparo d && (case arma of
        Mina ->  True
        _ -> False)
    ehMina (Barril _ _) = False

    -- | Verifica se a mina está numa das posições adjacentes
    checkMina :: Objeto -> [Posicao] -> (Bool, Maybe Posicao)
    checkMina _ [] = (False, Nothing)
    checkMina d@(Disparo posM _ _ _ _) (h:t) = if posM == h then (True, Just posM) else checkMina d t




-- * PROCESSAMENTO DE SUICÍDIO

-- | Devolve o tempo necessário para que todas as minhocas se suicidem no estado
-- Este tempo é calculado somando:
-- - Tempo para dinamite explodir
-- - Tempo para bazuca explodir 
-- - Tempo para registrar explosão
-- - Margem de segurança + número de minhocas
tickingBomb :: Estado -> Ticks
tickingBomb e = tempoArmas + numTerr + threshold

  where
    minhocas = minhocasEstado e
    (numTerr, terroristas) = getTerroristas minhocas
    armas = getArmasSuicidio terroristas
    tempoArmas = getGunTimeCount (nub armas)
    threshold = 10 + length minhocas -- Valor default para segurança

    -- | Identifica as minhocas que podem suicidar-se
    -- | Devolve o número de terroristas e a lista de minhocas que podem suicidar-se
    getTerroristas :: [Minhoca] -> (Int, [Minhoca])
    getTerroristas [] = (0, [])
    getTerroristas (h:t) =
      let (cont, lista) = getTerroristas t
          (_, podeSuicidar) = canSuicide h
      in if podeSuicidar
        then (cont + 1, h : lista)
        else (cont, lista)

    -- | Devolve as armas usadas para suicídio
    getArmasSuicidio :: [Minhoca] -> [TipoArma]
    getArmasSuicidio [] = []
    getArmasSuicidio (h:t) = let (arma, _) = canSuicide h
                            in fromJust arma : getArmasSuicidio t

    -- | Devolve o tempo total necessário para as armas explodirem
    getGunTimeCount :: [TipoArma] -> Int
    getGunTimeCount [] = 0
    getGunTimeCount (h:t) = case h of
      Bazuca -> 1 + getGunTimeCount t
      Dinamite -> 5 + getGunTimeCount t

    -- | Verifica se uma minhoca pode suicidar-se
    -- | Devolve a arma disponível (se houver) e se pode suicidar-se
    canSuicide :: Minhoca -> (Maybe TipoArma, Bool)
    canSuicide m
      | dinamiteMinhoca m > 0 = (Just Dinamite, True)
      | bazucaMinhoca m > 0 = (Just Bazuca, True)
      | otherwise = (Nothing, False)

-- | Função que determina se uma minhoca pode saltar para o suicidio
canBungeJump :: Mapa -> Posicao -> (Bool,Maybe Direcao)
canBungeJump mapa pos
  | isPathFree posDireita = (True, Just Este)
  | isPathFree posEsquerda = (True, Just Oeste)
  | otherwise = (False, Nothing)

  where

    posDireita = getPosicoesAbaixo (movePosicao Este pos)
    posEsquerda = getPosicoesAbaixo (movePosicao Oeste pos)

    isPathFree :: [Posicao] -> Bool
    isPathFree [] = True
    isPathFree (h:t) = ePosicaoMapaLivre h mapa && isPathFree t

    -- | Devolve todas as posições dentro da matriz abaixo de uma posição
    getPosicoesAbaixo :: Posicao -> [Posicao]
    getPosicoesAbaixo (y, x) =
      let (dY, _) = getDimensoesMatriz mapa
          anda linha =
            if linha >= dY
              then []
              else (linha, x) : anda (linha + 1)
      in anda (y + 1)

-- | Funcao que determina se uma minhoca pode ir para o vazio
-- Tem algumas limitacoes, mas a logica é parecida com a canBungeJump 
canFollowVoid :: Mapa -> Posicao -> (Bool,Maybe Direcao)
canFollowVoid mapa pos
  | isPathFree posicoes = (True, Just (getCloser pos posicoes))
  | otherwise = (False, Nothing)

  where

    posicoes = getPosicoesLaterais pos

    getCloser :: Posicao -> [Posicao] -> Direcao
    getCloser _ [] = Sul -- ! aviso
    getCloser p (h:t) = if distAtoB p (last t) >= distAtoB p h then getDir8 p h else getDir8 p (last t)

    isPathFree :: [Posicao] -> Bool
    isPathFree [] = True
    isPathFree (h:t) = ePosicaoMapaLivre h mapa && isPathFree t

    -- | Devolve todas as posições laterais de uma posicao dentro do mapa
    getPosicoesLaterais :: Posicao -> [Posicao]
    getPosicoesLaterais (y, x) =
        let (_, dX) = getDimensoesMatriz mapa
            todasColunas = [0..dX-1]
            semPosicaoAtual = filter (/= x) todasColunas
            posicoes = map (\coluna -> (y, coluna)) semPosicaoAtual
        in posicoes




-- * FUNÇÕES AUXILIARES

-- | Verifica se a minhoca tem munição de uma certa arma
gotAmmo :: Minhoca -> TipoArma -> Bool
gotAmmo minh tipo = encontraQuantidadeArmaMinhoca tipo minh > 0

-- | Devolve a direção que uma posição está em relação a outra
-- | (não funciona fora dos 8 eixos mas devolve na mesma o mais próximo)
getDir8 :: Posicao -> Posicao -> Direcao
getDir8 (y,x) (b,a)
  | y == b = if a < x then Oeste else Este
  | x == a = if b < y then Norte else Sul
  | b < y && a < x = Noroeste
  | b < y && a > x = Nordeste
  | b > y && a < x = Sudoeste
  | otherwise = Sudeste

-- | Calcula a distância euclidiana entre duas posições
distAtoB :: Posicao -> Posicao -> Double
distAtoB (x,y) (a,b) = sqrt ((dx * dx) + (dy * dy))
  where
    dx = fromIntegral (x - a)
    dy = fromIntegral (y - b)

-- | Extrai a componente horizontal (Este/Oeste) de uma direção
getXWay :: Direcao -> Direcao
getXWay d = case d of
  Noroeste  -> Oeste
  Sudoeste  -> Oeste
  Nordeste  -> Este
  Sudeste   -> Este
  _         -> d

































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

