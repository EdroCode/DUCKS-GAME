{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-|
Module      : Tarefa4
Description : Táctica automatizada para o jogo.

Módulo para a realização da Tarefa 4 de LI1\/LP1 em 2025\/26.

Este Bot utiliza diversos processos e algoritmos para conseguir obter 
a melhor jogada em certas situações.

= Lógica do Bot

Este bot implementa uma estratégia em duas fases distintas:

== Fase Normal

O bot seleciona minhocas circularmente e executa a seguinte sequência de prioridades:

    1. __Ataque Direto__: Se deteta inimigos ou barris na linha de visão, dispara a Bazuca
    2. __Ataque Estratégico__: Se tem Bazuca mas sem alvos visíveis, calcula a posição de impacto com maior dano ao terreno destrutível
    3. __Escavação__: Se tem Escavadora, remove blocos de terra adjacentes para aumentar o dano total feito.
    4. __Movimento Inteligente__: Move-se horizontalmente em direção à minhoca inimiga mais próxima

== Fase Suicídio

Quando o tempo está a acabar, o bot tenta maximizar o dano final:

    1. Dispara Mina ou Dinamite no próprio local (Se Tiver)
    2. Se não tiver explosivos, tenta Bazuca apontada para baixo
    3. Sem armas, procura outras formas de suicídio:
        
        * Move-se para água
        * Escava terra abaixo
        * Salta para fora do mapa
        * Move-se em direção a outras minhocas (O que causa eventos adicionais que podem levar a mais dano)


O bot prioriza causar dano contínuo através de ataques estratégicos, mantendo mobilidade 
com escavação, e termina com uma sequência suicida coordenada para maximizar destruição final.

= Flow Map do Bot

<https://www.mermaidchart.com/d/cd496e3a-b7e6-4575-86e8-b9b239f77e1d>
-}
module Tarefa4 where

import Data.Either

import Labs2025
import Tarefa2
import Tarefa3
import Tarefa0_2025 (ePosicaoEstadoLivre, existeMinhoca, existeBarril, verificaVida, encontraQuantidadeArmaMinhoca, ehDisparo, existeBarril, eMinhocaViva, ePosicaoMapaLivre, eTerrenoDestrutivel)
import Data.Maybe (fromJust, mapMaybe)
import Data.List



-- * Função Principal

{-| Gera uma sequência de jogadas com base num estado inicial.

Funcionamento:

* Produz exactamente 100 jogadas (ticks 0..99) aplicando iterativamente 'avancaTatica'
* Cada jogada é determinada pela análise do estado atual do jogo
* A sequência é revertida no final para manter a ordem cronológica

==__Exemplos:__

>>> take 5 (tatica estado)
[(0,Move Sul),(1,Dispara Bazuca Norte),...]

-}
tatica :: Estado -> [(NumMinhoca,Jogada)]
tatica e = reverse $ snd $ foldl avancaTatica (e,[]) [0..99]

-- * Funções Auxiliares

{-| Avança a táctica num único tick.

Funcionamento:

* Determina a próxima jogada com 'jogadaTatica'
* Aplica a jogada ao estado com 'avancaJogada'
* Acumula a jogada na lista de jogadas realizadas

==__Exemplos:__

>>> avancaTatica (estado,[]) 0
(novoEstado,[(0,Move Sul)])

-}
avancaTatica :: (Estado,[(NumMinhoca,Jogada)]) -> Ticks -> (Estado,[(NumMinhoca,Jogada)])
avancaTatica (e,js) tick = (avancaJogada j e,j:js)
    where j = jogadaTatica tick e

{-| Aplica uma jogada no estado e processa o avanço temporal.

Funcionamento:

* Aplica 'efetuaJogada' para obter o estado intermédio
* Avança o estado de cada minhoca com 'avancaMinhocaJogada'
* Avança objetos com 'avancaObjetoJogada' e acumula danos
* Aplica os danos acumulados com 'aplicaDanos'

-}
avancaJogada :: (NumMinhoca,Jogada) -> Estado -> Estado
avancaJogada (i,j) e@(Estado _ objetos minhocas) = foldr aplicaDanos e'' danoss''
    where
    e'@(Estado mapa' objetos' minhocas') = efetuaJogada i j e
    minhocas'' = map (avancaMinhocaJogada e') (zip3 [0..] minhocas minhocas')
    (objetos'',danoss'') = partitionEithers $ map (avancaObjetoJogada (e' { minhocasEstado = minhocas''}) objetos) (zip [0..] objetos')
    e'' = Estado mapa' objetos'' minhocas''

{-| Avança o relógio interno de uma minhoca se esta não executou uma jogada.

Funcionamento:

* Compara a posição antiga com a nova posição da minhoca
* Se as posições coincidirem, aplica 'avancaMinhoca' para processar efeitos temporais
* Caso contrário, mantém a minhoca no estado novo

-}
avancaMinhocaJogada :: Estado -> (NumMinhoca,Minhoca,Minhoca) -> Minhoca
avancaMinhocaJogada e (i,minhoca,minhoca') = if posicaoMinhoca minhoca == posicaoMinhoca minhoca'
    then avancaMinhoca e i minhoca'
    else minhoca'

{-| Avança um objecto no tempo se já existia anteriormente.

Funcionamento:

* Verifica se o objeto já existia na lista antiga
* Se foi criado pela última jogada, devolve 'Left objeto'' para manter o objeto sem alterações
* Caso contrário aplica 'avancaObjeto' e devolve os danos acumulados

-}
avancaObjetoJogada :: Estado -> [Objeto] -> (NumObjeto,Objeto) -> Either Objeto Danos
avancaObjetoJogada e objetos (i,objeto') = if elem objeto' objetos
    then avancaObjeto e i objeto'
    else Left objeto'

-- * Lógica de Decisão

{-| Decide a jogada a executar num dado tick (0..99).

Funcionamento:

* Seleciona minhocas válidas (com posição e vivas)
* Verifica o número de jogadas restantes e compara com 'tickingBomb'
* Se houver tempo suficiente:

@
  - Verifica se existe um alvo na linha de visão
  - Se sim, tenta disparar com 'Bazuca'
  - Caso contrário, tenta 'shootPhase' (avalia posições de impacto)
  - Se faltar munição, passa para 'digFase'
@

* Se o tempo estiver a acabar, força suicídio com 'forcaSuicidio'

==__Exemplos:__

>>> jogadaTatica 0 estado
(0,Dispara Bazuca Norte)

>>> jogadaTatica 95 estado
(1,Dispara Dinamite Norte)

-}
jogadaTatica :: Ticks -> Estado -> (NumMinhoca, Jogada)
jogadaTatica ticks e =
  case minhocasEstado e of
    [] -> defaultBotMove
    minhocas ->
      case getMinhocasValidasComIndices minhocas of
        [] -> defaultBotMove
        minhocasValidasIdx ->
          if jogadasRestantes > tickingBomb e
          then -- Jogadas Normais
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
                _ -> if gotAmmo minhoca Bazuca
                  then shootPhase e idOriginal minhoca
                  else digFase idOriginal minhoca
          else -- Trigger Suicidio
            let
              numMinhocasValidas = length minhocasValidasIdx
              i = ticks `mod` numMinhocasValidas
              (idOriginal, minhoca) = minhocasValidasIdx !! i
            in
              forcaSuicidio idOriginal minhoca
  where
    jogadasRestantes = 100 - ticks
    mapa = mapaEstado e

    -- | Retorna minhocas válidas e os seus índices originais
    getMinhocasValidasComIndices :: [Minhoca] -> [(Int, Minhoca)]
    getMinhocasValidasComIndices minhocas =
      let validasMinhocas = getMinhocasValidas minhocas
      in [(i, m) | (i, m) <- zip [0..] minhocas, m `elem` validasMinhocas]

    -- | Tenta forçar um suicídio utilizando a arma mais adequada disponível
    forcaSuicidio :: Int -> Minhoca -> (NumMinhoca, Jogada)
    forcaSuicidio i minhoca
      | gotAmmo minhoca Mina = (i, Dispara Mina Norte)
      | gotAmmo minhoca Dinamite = (i, Dispara Dinamite Norte)
      | gotAmmo minhoca Bazuca = (i, Dispara Bazuca Sul)
      | otherwise = suicidioSemArmas i minhoca

    -- | Verifica se existe um bloco ou barril adjacente com espaço livre acima
    podeSubirEmBloco :: Mapa -> [Objeto] -> Posicao -> Maybe Direcao
    podeSubirEmBloco _ objetos pos = 
      let posicoesAdjacentes = [(Este, movePosicao Este pos), 
                                (Oeste, movePosicao Oeste pos)]
          
          verificaPosicao :: (Direcao, Posicao) -> Maybe Direcao
          verificaPosicao (dir, p) = 
            let posAcima = movePosicao Norte p
                terrenoAtual = encontraPosicaoMatriz p mapa
                terrenoAcima = encontraPosicaoMatriz posAcima mapa
                temBarril = existeBarril p (filter (not . ehDisparo) objetos)
            in case (terrenoAtual, terrenoAcima) of
              (Just Terra, Just Ar) -> Just dir
              (Just Pedra, Just Ar) -> Just dir
              (Just Ar, Just Ar) | temBarril -> Just dir
              _ -> Nothing
      in case mapMaybe verificaPosicao posicoesAdjacentes of
          [] -> Nothing
          (d:_) -> Just d


          
    -- | Estratégia de suicídio quando a minhoca não possui armas
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
        
        podePularEmBloco = podeSubirEmBloco mapa (objetosEstado e) pos

      in
        case blocoAgua of
          Just a -> 
            case podePularEmBloco of
              Just dirBloco -> (i, Move dirBloco)
              Nothing -> (i, Move (getDir8 pos a))
          Nothing ->
            case blocoTerra of
              Just b -> 
                if not (ePosicaoMatrizValida (movePosicao Sul b) mapa) && gotAmmo minhoca Escavadora 
                then (i, Dispara Escavadora (getDir8 pos b))
                else case podePularEmBloco of
                      Just dirBloco -> (i, Move dirBloco)
                      Nothing -> complexBotMove e (i, minhoca)

              Nothing -> 
                case blocoNegro of
                  Just posInvalida -> 
                    case podePularEmBloco of
                      Just dirBloco -> (i, Move dirBloco)
                      Nothing -> (i, Move (getDir8 pos posInvalida))
                  Nothing -> 
                    case (podeSaltar, dirSalto) of
                      (True, Just d) -> (i, Move d)
                      _ -> case podePularEmBloco of
                            Just dirBloco -> (i, Move dirBloco)
                            Nothing -> complexBotMove e (i, minhoca)

    -- | Fase de escavação / saída de bloqueio
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
                      case blocoAr of
                        Just a -> (i, Move (getDir8 pos a))
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
                      case blocoAr of
                        Just a -> (i, Move (getDir8 pos a))
                        Nothing -> defaultBotMove
                    else complexBotMove e (i, minhoca)

    -- | Fase de disparo: escolhe a melhor posição para disparar com a 'Bazuca'
    shootPhase :: Estado -> Int -> Minhoca -> (NumMinhoca, Jogada)
    shootPhase est i minhoca =  (i, Dispara Bazuca (getDir8 pos melhorPosicao))
      where
        m = mapaEstado est
        pos = case posicaoMinhoca minhoca of 
                Just a -> a
                
        posPossiveis = getPosicoesBazuca m pos
        danosComPosicao = map (\p -> (p, calculaDanoTotal m p)) posPossiveis
        melhorPosicao = maximoPorDano danosComPosicao

        calculaDanoTotal :: Mapa -> Posicao -> Int
        calculaDanoTotal mp poss = 
            let danos = calculaExplosao poss 5
                danosTerreno = filtraDanoTerreno danos mp
            in somaDanos danosTerreno

        filtraDanoTerreno :: Danos -> Mapa -> Danos
        filtraDanoTerreno [] _ = []
        filtraDanoTerreno ((posDano, dmg):t) mp = 
            case encontraPosicaoMatriz posDano mp of
                Just peca -> if eTerrenoDestrutivel peca 
                            then (posDano, dmg) : filtraDanoTerreno t mp 
                            else filtraDanoTerreno t mp
                Nothing -> filtraDanoTerreno t mp

        somaDanos :: Danos -> Int
        somaDanos = sum . map snd

        maximoPorDano :: [(Posicao, Int)] -> Posicao
        maximoPorDano [(p, _)] = p
        maximoPorDano ((p1, d1):(p2, d2):t) = 
            if d1 >= d2 
            then maximoPorDano ((p1, d1):t)
            else maximoPorDano ((p2, d2):t)

{-| Lógica de movimento mais complexa quando não existe acção óbvia.

Funcionamento:

* Procura a minhoca mais próxima válida
* Tenta mover na sua direção horizontal
* Se a posição destino estiver ocupada, tenta uma direção alternativa
* Se não houver minhocas próximas, move-se para Sul por defeito

==__Exemplos:__

>>> complexBotMove estado (0,minhoca)
(0,Move Este)

-}
complexBotMove :: Estado -> (Int, Minhoca) -> (NumMinhoca, Jogada)
complexBotMove e (i, minhoca) = case minhocaProx of
  Just m -> let posM = case posicaoMinhoca m of Just a -> a
                dirMinhoca = getDir8 pos posM

                posNova = movePosicao dirMinhoca pos
                dirX = getXWay dirMinhoca

                fAux :: Direcao -> Direcao
                fAux d = case d of
                  Este -> Nordeste
                  Oeste -> Noroeste

            in if ePosicaoEstadoLivre posNova e 
              then (i, Move dirX)
              else (i, Move (fAux dirX))

  Nothing -> (i, Move Sul)
  where
    pos = case posicaoMinhoca minhoca of Just a -> a
    minhocasValidas = filter (/= minhoca) (getMinhocasValidas (minhocasEstado e))
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

-- * Algoritmos de Escolha de Minhocas

{-| Escolhe a minhoca com menor vida entre as válidas.

Funcionamento:

* Filtra as minhocas válidas com 'getMinhocasValidas'
* Compara a vida de cada minhoca
* Retorna o índice relativo (0..n) da minhoca com menos vida

==__Exemplos:__

>>> escolherMinhoca [minhoca1,minhoca2,minhoca3]
1

-}
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

{-| Filtra as minhocas válidas (com posição e vivas).

Funcionamento:

* Verifica se a posição da minhoca não é 'Nothing'
* Verifica se a minhoca está viva com 'eMinhocaViva'
* Retorna apenas as minhocas que satisfazem ambas as condições

==__Exemplos:__

>>> getMinhocasValidas [minhoca1,minhoca2Morta,minhoca3SemPosicao]
[minhoca1]

-}
getMinhocasValidas :: [Minhoca] -> [Minhoca]
getMinhocasValidas [] = []
getMinhocasValidas (h:t) =
  if posicaoMinhoca h /= Nothing && eMinhocaViva h
    then h : getMinhocasValidas t
    else getMinhocasValidas t

-- * Deteção de Alvos

{-| Detecta alvos (minhocas ou barris) na linha de visão de uma posição.

Funcionamento:

* Obtém todas as posições visíveis com 'getPosicoesBazuca'
* Verifica se existem minhocas vivas nessas posições
* Verifica se existem barris nessas posições
* Escolhe o alvo mais próximo com 'escolheMaisPerto'

==__Exemplos:__

>>> minhocaOnSight estado (2,3)
(True,Just (2,5))

>>> minhocaOnSight estado (0,0)
(False,Nothing)

-}
minhocaOnSight :: Estado -> Posicao -> (Bool, Maybe Posicao)
minhocaOnSight e pos =
    let mapa          = mapaEstado e
        posicoesLV    = getPosicoesBazuca mapa pos
        minhocasVivas = filter verificaVida (minhocasEstado e)

        barris = getBarris (objetosEstado e)

        minhocasInSight = filter (`existeMinhoca` minhocasVivas) posicoesLV
        barrisInSight = filter (`existeBarril` barris) posicoesLV

        maisPerto = escolheMaisPerto pos (minhocasInSight ++ barrisInSight)

        escolheMaisPerto :: Posicao -> [Posicao] -> Maybe Posicao
        escolheMaisPerto _ []     = Nothing
        escolheMaisPerto p (x:xs) = Just (foldl (comparaDist p) x xs)

        comparaDist :: Posicao -> Posicao -> Posicao -> Posicao
        comparaDist p melhor atual =
            if distAtoB p atual < distAtoB p melhor then atual else melhor

        getBarris :: [Objeto] -> [Objeto]
        getBarris [] = []
        getBarris (h:t) = if ehDisparo h then getBarris t else h : getBarris t

    in (not (null minhocasInSight && null barrisInSight), maisPerto)

-- * Validação e Dimensões

{-| Verifica se a posição inferior ao bloco está dentro da matriz.

Funcionamento:

* Move a posição uma casa para Sul
* Verifica se a nova posição é válida na matriz

==__Exemplos:__

>>> lastBlockValid mapa (5,3)
False

>>> lastBlockValid mapa (2,3)
True

-}
lastBlockValid :: Mapa -> Posicao -> Bool
lastBlockValid mapa pos = let posInferior = movePosicao Sul pos in ePosicaoMatrizValida posInferior mapa

{-| Devolve as dimensões da matriz (altura, largura).

Funcionamento:

* Conta o número de linhas
* Conta o número de colunas na primeira linha

==__Exemplos:__

>>> getDimensoesMatriz [[Ar,Ar],[Ar,Ar],[Ar,Ar]]
(3,2)

-}
getDimensoesMatriz :: Mapa -> (Int, Int)
getDimensoesMatriz m = (length m, length (head m))

-- * Cálculo de Posições

{-| Devolve todas as posições visíveis nas 8 direções até ao limite da matriz.

Funcionamento:

* Para cada uma das 8 direções
* Calcula todas as posições na direção até sair da matriz
* Ordena e concatena todas as posições encontradas

==__Exemplos:__

>>> getPosicoesBazuca mapa (2,2)
[(0,0),(0,1),(0,2),(0,3),(0,4),(1,1),(1,2),...] 

-}
getPosicoesBazuca :: Mapa -> Posicao -> [Posicao]
getPosicoesBazuca mapa pos = sort (concatMap (posicoesDirecao mapa pos) direcoes)
    where
        direcoes :: [Direcao]
        direcoes = [Norte, Sul, Este, Oeste,
                    Nordeste, Noroeste, Sudeste, Sudoeste]

        posicoesDirecao :: Mapa -> Posicao -> Direcao -> [Posicao]
        posicoesDirecao m p dir =
          let (dY, dX) = getDimensoesMatriz m
              anda (y,x) =
                if y < 0 || x < 0 || y >= dY || x >= dX
                  then []
                  else (y,x) : anda (movePosicao dir (y,x))
          in anda (movePosicao dir p)

{-| Devolve as posições válidas adjacentes (quadrado 3x3) a uma posição.

Funcionamento:

* Para cada uma das 8 direções adjacentes
* Verifica se a posição está dentro da matriz
* Retorna apenas as posições válidas

==__Exemplos:__

>>> getPosicoesSquare mapa (2,2)
[(1,1),(1,2),(1,3),(2,1),(2,3),(3,1),(3,2),(3,3)]

-}
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

{-| Devolve todas as posições adjacentes (quadrado 3x3) a uma posição.

Funcionamento:

* Para cada uma das 8 direções adjacentes
* Calcula a posição nessa direção
* Inclui tanto posições válidas como inválidas (fora da matriz)

==__Exemplos:__

>>> getPosicoesSquareInv (0,0)
[(-1,-1),(-1,0),(-1,1),(0,-1),(0,1),(1,-1),(1,0),(1,1)]

-}
getPosicoesSquareInv :: Posicao -> [Posicao]
getPosicoesSquareInv pos = sort (concatMap (posicoesDirecao pos) direcoes)
  where
    direcoes :: [Direcao]
    direcoes = [Norte, Sul, Este, Oeste, Nordeste, Noroeste, Sudeste, Sudoeste]

    posicoesDirecao :: Posicao -> Direcao -> [Posicao]
    posicoesDirecao p dir =
      let (y, x) = movePosicao dir p
      in [(y, x)]

-- * Procura de Terreno

{-| Procura o primeiro terreno do tipo dado numa lista de posições.

Funcionamento:

* Percorre a lista de posições
* Verifica o terreno em cada posição
* Retorna a primeira posição que contém o terreno procurado

==__Exemplos:__

>>> getPosTer mapa Terra [(1,1),(2,2),(3,3)]
Just (2,2)

>>> getPosTer mapa Agua [(1,1),(2,2)]
Nothing

-}
getPosTer :: Mapa -> Terreno -> [Posicao] -> Maybe Posicao
getPosTer _ _ [] = Nothing
getPosTer mapa tipo (h:t) = case encontraPosicaoMatriz h mapa of
  Just a -> if tipo == a then Just h else getPosTer mapa tipo t
  Nothing -> Nothing

{-| Verifica se existe uma posição inválida numa lista de posições.

Funcionamento:

* Percorre a lista de posições
* Verifica se cada posição é válida na matriz
* Retorna a primeira posição inválida encontrada

==__Exemplos:__

>>> getPosInv mapa [(1,1),(2,2),(-1,5)]
Just (-1,5)

-}
getPosInv :: Mapa -> [Posicao] -> Maybe Posicao
getPosInv _  [] = Nothing
getPosInv mapa (h:t) = if ePosicaoMatrizValida h mapa then getPosInv mapa t else Just h

{-| Procura a primeira posição livre (Ar ou Agua) numa lista de posições.

Funcionamento:

* Percorre a lista de posições
* Verifica o terreno em cada posição
* Retorna a primeira posição que contém 'Ar' ou 'Agua'

==__Exemplos:__

>>> getPosLivre mapa [(2,2),(1,1),(0,0)]
Just (0,0)

-}
getPosLivre:: Mapa -> [Posicao] -> Maybe Posicao
getPosLivre _ [] = Nothing
getPosLivre mapa (h:t) = case encontraPosicaoMatriz h mapa of
  Just a -> if a == Ar || a == Agua then Just h else getPosLivre mapa t
  Nothing -> Nothing

{-| Verifica se existe uma mina adjacente a uma posição.

Funcionamento:

* Obtém todas as posições adjacentes
* Filtra os objetos do tipo 'Mina'
* Verifica se alguma mina está numa posição adjacente

==__Exemplos:__

>>> temMinaPos (2,2) mapa objetos
(True,Just (2,3))

>>> temMinaPos (0,0) mapa []
(False,Nothing)

-}
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
    ehMina (Barril _ _) = False

    checkMina :: Objeto -> [Posicao] -> (Bool, Maybe Posicao)
    checkMina _ [] = (False, Nothing)
    checkMina d@(Disparo posM _ _ _ _) (h:t) = if posM == h then (True, Just posM) else checkMina d t

-- * Processamento de Suicídio

{-| Calcula o tempo necessário para que as minhocas suicidas completem as acções.

Funcionamento:

* Identifica as minhocas que podem suicidar-se
* Soma o tempo de detonação de cada arma
* Adiciona uma margem de segurança baseada no número de minhocas

==__Exemplos:__

>>> tickingBomb estado
25

-}
tickingBomb :: Estado -> Ticks
tickingBomb e = tempoArmas + numTerr + threshold
  where
    minhocas = minhocasEstado e
    (numTerr, terroristas) = getTerroristas minhocas
    armas = getArmasSuicidio terroristas
    tempoArmas = getGunTimeCount (nub armas)
    threshold = 10 + length minhocas

    getTerroristas :: [Minhoca] -> (Int, [Minhoca])
    getTerroristas [] = (0, [])
    getTerroristas (h:t) =
      let (cont, lista) = getTerroristas t
          (_, podeSuicidar) = canSuicide h
      in if podeSuicidar
        then (cont + 1, h : lista)
        else (cont, lista)

    getArmasSuicidio :: [Minhoca] -> [TipoArma]
    getArmasSuicidio [] = []
    getArmasSuicidio (h:t) = let (arma, _) = canSuicide h
                            in fromJust arma : getArmasSuicidio t

    getGunTimeCount :: [TipoArma] -> Int
    getGunTimeCount [] = 0
    getGunTimeCount (h:t) = case h of
      Bazuca -> 1 + getGunTimeCount t
      Dinamite -> 5 + getGunTimeCount t
      _ -> getGunTimeCount t

    canSuicide :: Minhoca -> (Maybe TipoArma, Bool)
    canSuicide m
      | dinamiteMinhoca m > 0 = (Just Dinamite, True)
      | bazucaMinhoca m > 0 = (Just Bazuca, True)
      | otherwise = (Nothing, False)

{-| Determina se uma minhoca pode saltar para uma posição segura abaixo.

Funcionamento:

* Verifica as posições abaixo à direita e à esquerda
* Confirma se o caminho está livre (sem terreno opaco)
* Retorna a direção disponível para o salto

==__Exemplos:__

>>> canBungeJump mapa (2,2)
(True,Just Este)

>>> canBungeJump mapa (2,2)
(False,Nothing)

-}
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

    getPosicoesAbaixo :: Posicao -> [Posicao]
    getPosicoesAbaixo (y, x) =
      let (dY, _) = getDimensoesMatriz mapa
          anda linha =
            if linha >= dY
              then []
              else (linha, x) : anda (linha + 1)
      in anda (y + 1)

{-| Determina se existe um caminho lateral para cair no vazio.

Funcionamento:

* Procura colunas laterais com posições livres
* Verifica se o caminho está livre de terreno opaco
* Escolhe a direção mais próxima com 'getCloser'

==__Exemplos:__

>>> canFollowVoid mapa (2,2)
(True,Just Este)

>>> canFollowVoid mapa (2,2)
(False,Nothing)

-}
canFollowVoid :: Mapa -> Posicao -> (Bool,Maybe Direcao)
canFollowVoid mapa pos
  | isPathFree posicoes = (True, Just (getCloser pos posicoes))
  | otherwise = (False, Nothing)
  where
    posicoes = getPosicoesLaterais pos

    getCloser :: Posicao -> [Posicao] -> Direcao
    getCloser _ [] = Sul
    getCloser p (h:t) = if distAtoB p (last t) >= distAtoB p h then getDir8 p h else getDir8 p (last t)

    isPathFree :: [Posicao] -> Bool
    isPathFree [] = True
    isPathFree (h:t) = ePosicaoMapaLivre h mapa && isPathFree t

    getPosicoesLaterais :: Posicao -> [Posicao]
    getPosicoesLaterais (y, x) =
        let (_, dX) = getDimensoesMatriz mapa
            todasColunas = [0..dX-1]
            semPosicaoAtual = filter (/= x) todasColunas
            posicoesD = map (\coluna -> (y, coluna)) semPosicaoAtual
        in posicoesD

-- * Funções Auxiliares Gerais

{-| Verifica se a minhoca tem munição de um determinado tipo de arma.

Funcionamento:

* Utiliza 'encontraQuantidadeArmaMinhoca' para obter a quantidade
* Retorna True se a quantidade for maior que 0

==__Exemplos:__

>>> gotAmmo minhoca Bazuca
True

>>> gotAmmo minhoca Jetpack
False

-}
gotAmmo :: Minhoca -> TipoArma -> Bool
gotAmmo minh tipo = encontraQuantidadeArmaMinhoca tipo minh > 0

{-| Calcula a direção dos 8 eixos entre duas posições.

Funcionamento:

* Compara as coordenadas das duas posições
* Determina a direção cardinal ou diagonal
* Retorna a direção mais próxima quando o alvo não está alinhado

==__Exemplos:__

>>> getDir8 (2,2) (0,2)
Norte

>>> getDir8 (2,2) (0,4)
Nordeste

-}
getDir8 :: Posicao -> Posicao -> Direcao
getDir8 (y,x) (b,a)
  | y == b = if a < x then Oeste else Este
  | x == a = if b < y then Norte else Sul
  | b < y && a < x = Noroeste
  | b < y && a > x = Nordeste
  | b > y && a < x = Sudoeste
  | otherwise = Sudeste

{-| Distância euclidiana entre duas posições.

Funcionamento:

* Calcula a diferença em cada eixo
* Aplica o teorema de Pitágoras
* Retorna a distância como Double

==__Exemplos:__

>>> distAtoB (0,0) (3,4)
5.0

>>> distAtoB (2,2) (2,5)
3.0

-}
distAtoB :: Posicao -> Posicao -> Double
distAtoB (x,y) (a,b) = sqrt ((dx * dx) + (dy * dy))
  where
    dx = fromIntegral (x - a)
    dy = fromIntegral (y - b)

{-| Extrai a componente horizontal (Este/Oeste) de uma direção.

Funcionamento:

* Recebe uma direção (pode ser diagonal)
* Retorna apenas a componente horizontal
* Para direções cardeais verticais, retorna Este por defeito

==__Exemplos:__

>>> getXWay Noroeste
Oeste

>>> getXWay Sudeste
Este

>>> getXWay Norte
Este

-}
getXWay :: Direcao -> Direcao
getXWay d = case d of
  Noroeste  -> Oeste
  Sudoeste  -> Oeste
  Nordeste  -> Este
  Sudeste   -> Este
  _         -> Este

