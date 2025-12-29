{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-|
Module      : AvancaEstado
Description : Avançar tempo do jogo.

Variante da Tarefa4 para a DLC
-}
module AvancaEstado where


import Data.Either
import EfetuaJogada
import DataDLC( posicaoObjeto,TerrenoDLC(ArDLC, AguaDLC, Lava), fireDamage, VidaMinhocaDLC(MortaDLC, VivaDLC), MapaDLC,EstadoDLC(EstadoDLC), MinhocaDLC, ObjetoDLC (AmmoPack, posicaoHP), minhocasEstadoDLC, posicaoMinhocaDLC, vidaMinhocaDLC, burningCounter, posicaoDisparoDLC, direcaoDisparoDLC, tempoDisparoDLC, tipoDisparoDLC, donoDisparoDLC, posicaoBarrilDLC, explodeBarrilDLC, ObjetoDLC, ObjetoDLC(DisparoDLC, BarrilDLC, HealthPack), minhocasEstadoDLC, objetosEstadoDLC, mapaEstadoDLC, TipoArmaDLC(MinaDLC, BazucaDLC, DinamiteDLC, FlameTrower))
import Labs2025(NumMinhoca,Posicao, NumObjeto, Direcao(Norte,Este,Oeste,Sul,Nordeste,Noroeste,Sudoeste,Sudeste))
import Auxiliar


-- * Tipos Auxiliares
type Dano = Int
type Danos = [(Posicao,Dano)]

-- * Função Principal

{-| Avança o 'Estado' do jogo em um tick('Ticks').

Funcionamento:

* Atualiza os componentes de cada 'Minhoca'
* Atualiza os componentes de cada 'Objeto'
* Cria um novo 'Estado' contendo as minhocas e os objetos atualizados de acordo com o dano recebido



-}
avancaEstado :: EstadoDLC -> EstadoDLC
avancaEstado e@(EstadoDLC mapa objetos minhocas armSel mSel) = foldr aplicaDanos e' danoss
    where
    minhocas' = zipWith (curry (uncurry $ avancaMinhoca e)) [0..] minhocas
    (objetos',danoss) = partitionEithers $ zipWith (curry (uncurry $ avancaObjeto $ e { minhocasEstadoDLC = minhocas' })) [0..] objetos
    e' = EstadoDLC mapa objetos' minhocas' armSel mSel

-- * Funções Auxiliares

{- |Para um dado 'Estado', dado o índice('NumMinhoca') de uma 'Minhoca' na lista de minhocas e o 'Estado' dessa minhoca, retorna o novo 'Estado' da 'Minhoca' no próximo tick('Ticks').

Funcionamento:

* Verifica se a 'Minhoca' tem posição

* Verifica a 'Vida' da 'Minhoca'

* Calcula a nova posição da 'Minhoca' (cair ou mover)

* Verifica o 'Terreno' na nova 'Posicao' e atualiza a 'Minhoca' conforme o novo 'Terreno'

-}
avancaMinhoca :: EstadoDLC -> NumMinhoca -> MinhocaDLC -> MinhocaDLC
avancaMinhoca e _ minhoca=
  case posicaoMinhocaDLC minhoca of
    Nothing -> minhoca
    Just pos ->
      let
        posicaoTentativa = if estaNoSolo pos mapa( minhocasEstadoDLC e) || existeMinhocaViva (movePosicao Sul pos) (minhocasEstadoDLC e)
                              then pos
                              else movePosicao Sul pos

        -- Se a posição tentativa for inválida, a minhoca morre e desaparece
        posicaoFinalValida = ePosicaoMatrizValida posicaoTentativa mapa
        terreno = encontraPosicaoMatriz posicaoTentativa mapa


      in if posicaoFinalValida
            then case vidaMinhocaDLC minhoca of
                MortaDLC -> minhoca { posicaoMinhocaDLC = Just posicaoTentativa, vidaMinhocaDLC = MortaDLC, burningCounter = 0 }
                VivaDLC 0 -> case terreno of
                  Just AguaDLC -> minhoca { posicaoMinhocaDLC = Just posicaoTentativa, vidaMinhocaDLC = MortaDLC, burningCounter = 0 }
                  Just Lava -> minhoca { posicaoMinhocaDLC = Just posicaoTentativa, vidaMinhocaDLC = MortaDLC, burningCounter = 0 }
                  Just ArDLC   -> minhoca { posicaoMinhocaDLC = Just posicaoTentativa, vidaMinhocaDLC = MortaDLC, burningCounter = 0 }
                  _         -> minhoca { posicaoMinhocaDLC = Just posicaoTentativa, vidaMinhocaDLC = MortaDLC, burningCounter = 0 }
                VivaDLC v -> case existeObjeto posicaoTentativa objetos of
                  Just (DisparoDLC _ _ tipo _ _)  -> if tipo == FlameTrower then minhoca { posicaoMinhocaDLC = Just posicaoTentativa, vidaMinhocaDLC = VivaDLC (v-fireDamage), burningCounter = 2}
                    else case terreno of
                    Just AguaDLC -> minhoca { posicaoMinhocaDLC = Just posicaoTentativa, vidaMinhocaDLC =  MortaDLC, burningCounter = 0}
                    Just Lava -> minhoca { posicaoMinhocaDLC = Just posicaoTentativa, vidaMinhocaDLC = VivaDLC (v-fireDamage), burningCounter = 5}
                    Just ArDLC   -> minhoca { posicaoMinhocaDLC = Just posicaoTentativa, vidaMinhocaDLC = if burningCounter minhoca > 0 then VivaDLC (v-fireDamage) else VivaDLC v, burningCounter = if burningCounter minhoca > 0 then burningCounter minhoca - 1 else 0 }
                    _         -> minhoca { posicaoMinhocaDLC = Just posicaoTentativa, vidaMinhocaDLC = VivaDLC v, burningCounter = if burningCounter minhoca > 0 then burningCounter minhoca - 1 else 0 }
                  
                  Just (HealthPack _ hp) -> case terreno of
                    Just AguaDLC -> minhoca { posicaoMinhocaDLC = Just posicaoTentativa, vidaMinhocaDLC =  MortaDLC, burningCounter = 0}
                    Just Lava -> minhoca { posicaoMinhocaDLC = Just posicaoTentativa, vidaMinhocaDLC = VivaDLC (v-fireDamage + hp), burningCounter = 5}
                    Just ArDLC   -> minhoca { posicaoMinhocaDLC = Just posicaoTentativa, vidaMinhocaDLC = if burningCounter minhoca > 0 then VivaDLC (v-fireDamage+hp) else VivaDLC (v+hp), burningCounter = if burningCounter minhoca > 0 then burningCounter minhoca - 1 else 0 }
                    _         -> minhoca { posicaoMinhocaDLC = Just posicaoTentativa, vidaMinhocaDLC = if burningCounter minhoca > 0 then VivaDLC (v-fireDamage+hp) else VivaDLC (v+hp), burningCounter = if burningCounter minhoca > 0 then burningCounter minhoca - 1 else 0 }
                  
                  Just (AmmoPack _ ammo ammotype) -> let 
                      minhocaAtualizada = atualizaQuantidadeArmaMinhoca ammotype minhoca ammo
                    
                    in case terreno of
                    Just AguaDLC -> minhocaAtualizada { posicaoMinhocaDLC = Just posicaoTentativa, vidaMinhocaDLC =  MortaDLC, burningCounter = 0}
                    Just Lava -> minhocaAtualizada { posicaoMinhocaDLC = Just posicaoTentativa, vidaMinhocaDLC = VivaDLC (v-fireDamage), burningCounter = 5}
                    Just ArDLC   -> minhocaAtualizada { posicaoMinhocaDLC = Just posicaoTentativa, vidaMinhocaDLC = if burningCounter minhoca > 0 then VivaDLC (v-fireDamage) else VivaDLC v, burningCounter = if burningCounter minhoca > 0 then burningCounter minhoca - 1 else 0 }
                    _         -> minhocaAtualizada { posicaoMinhocaDLC = Just posicaoTentativa, vidaMinhocaDLC = VivaDLC v, burningCounter = if burningCounter minhoca > 0 then burningCounter minhoca - 1 else 0 }
                  
                  _ -> case terreno of
                    Just AguaDLC -> minhoca { posicaoMinhocaDLC = Just posicaoTentativa, vidaMinhocaDLC =  MortaDLC, burningCounter = 0}
                    Just Lava -> minhoca { posicaoMinhocaDLC = Just posicaoTentativa, vidaMinhocaDLC = VivaDLC (v-fireDamage), burningCounter = 5}
                    Just ArDLC   -> minhoca { posicaoMinhocaDLC = Just posicaoTentativa, vidaMinhocaDLC = if burningCounter minhoca > 0 then VivaDLC (v-fireDamage) else VivaDLC v, burningCounter = if burningCounter minhoca > 0 then burningCounter minhoca - 1 else 0 }
                    _         -> minhoca { posicaoMinhocaDLC = Just posicaoTentativa, vidaMinhocaDLC = VivaDLC v, burningCounter = if burningCounter minhoca > 0 then burningCounter minhoca - 1 else 0 }
                  
            else minhoca { posicaoMinhocaDLC = Nothing, vidaMinhocaDLC = MortaDLC }
          
  where
    mapa = mapaEstadoDLC e
    objetos = objetosEstadoDLC e

{- | Para um dado 'Estado', dado o índice('NumObjeto') de um 'Objeto' na lista de objetos e o estado desse objeto, retorna o novo estado do objeto no próximo tick ou, caso o objeto exploda, uma lista de posições afetadas com o dano associado. Cada 'Objeto' possui um comportamento diferente

Funcinamento Geral:

* Recebe o 'Estado', o índice do 'Objeto' e o 'Objeto' atual

* Para cada tipo de 'Objeto', aplica a lógica específica:

==__'BarrilDLC':__

* Se o 'BarrilDLC' não está a explodir:
  * Verifica se está no solo ou em água
    * Se não estiver, marca o 'BarrilDLC' para explodir
    * Caso contrário, mantém o 'BarrilDLC' inalterado

==__'BazucaDLC':__
* Move o 'DisparoDLC' na direção especificada
* Verifica se a nova posição é válida e livre
  * Se for, atualiza a posição do 'DisparoDLC'
  * Caso contrário, calcula a explosão na posição atual

==__'Mina':__
* Verifica o tempo restante da 'Mina'
  * Se acabar o tempo, calcula a explosão na posição atual
  * Se o tempo for maior que 0:

  @
  -Verifica se está no solo ou em água
    -Se não estiver, move a mina para baixo e ajusta a direção
    -Caso contrário, mantém a mina na posição atual
  @

  * Se o tempo for 'Nothing':

  @
  -Verifica se há outras minhocas no raio de explosão (exceto o dono)
    -Se não houver, mantém a mina inalterada
    -Caso contrário, inicia o temporizador da mina
  @

==__'Dinamite':__
* Verifica o tempo restante da 'Dinamite'
  * Se acabar o tempo, calcula a explosão na posição atual
  * Se o tempo for maior que 0:

  @
  -Verifica se está no solo
    -Se não estiver, move a dinamite para baixo ou roda a posição dependendo da direção
    -Caso contrário, mantém a dinamite na posição atual
  @



-}

avancaObjeto :: EstadoDLC -> NumObjeto -> ObjetoDLC -> Either ObjetoDLC Danos
avancaObjeto e _ o = case o of
  BarrilDLC posBarril explode ->
    if not explode
      then
        if not (estaNoSolo posBarril mapa minhocas) || estaEmAgua posBarril mapa || estaEmLava posBarril mapa -- esta no ar ou em agua
          then Left (BarrilDLC { posicaoBarrilDLC = posBarril, explodeBarrilDLC = True })
          else Left o
      else Right (calculaExplosao posBarril 5)

  HealthPack pos _ -> let 
                        novaPos = movePosicao Sul pos
                      in if ePosicaoEstadoLivre novaPos e then Left o {posicaoHP = novaPos} else if ePosicaoEstadoLivre pos e then Left o else Right []
  AmmoPack pos _ _ -> if ePosicaoEstadoLivre pos e && estaNoSolo pos mapa minhocas then Left o else Right []


  DisparoDLC pos dir tipo tempo dono -> case tipo of

      BazucaDLC ->
        let posNova = movePosicao dir pos
            tempoNovo = case tempo of
              Just t  -> Just (t - 1)
              Nothing -> Nothing
        in (if ePosicaoEstadoLivre pos e && ePosicaoMatrizValida posNova mapa then Left (DisparoDLC{posicaoDisparoDLC = posNova, direcaoDisparoDLC = dir, tipoDisparoDLC = tipo, tempoDisparoDLC = tempoNovo, donoDisparoDLC = dono}) else Right (calculaExplosao pos 5))

      FlameTrower ->
        let posNova = movePosicao dir pos
            tempoNovo = case tempo of
              Just t  -> Just (t - 1)
              Nothing -> Nothing
        in (if ePosicaoEstadoLivre pos e && ePosicaoMatrizValida posNova mapa then Left (DisparoDLC{posicaoDisparoDLC = posNova, direcaoDisparoDLC = dir, tipoDisparoDLC = tipo, tempoDisparoDLC = tempoNovo, donoDisparoDLC = dono}) else Right [])

      MinaDLC ->
        case tempo of
          Just 0 -> Right (calculaExplosao pos 3)
          Just _ ->
            let
              tempoNovo = case tempo of
                Just t  -> Just (t - 1)
                Nothing -> Nothing
            in
              if ePosicaoMatrizValida pos mapa
                then if not (estaNoSolo pos mapa minhocas) || estaEmAgua pos mapa
                  then
                    if dir == Sul
                      then Left (DisparoDLC { posicaoDisparoDLC = movePosicao dir pos, direcaoDisparoDLC = Norte, tipoDisparoDLC = tipo, tempoDisparoDLC = tempoNovo, donoDisparoDLC = dono })
                      else Left (DisparoDLC { posicaoDisparoDLC = movePosicao Sul pos, direcaoDisparoDLC = Norte, tipoDisparoDLC = tipo, tempoDisparoDLC = tempoNovo, donoDisparoDLC = dono })
                  else Left (DisparoDLC { posicaoDisparoDLC = pos, direcaoDisparoDLC = Norte, tipoDisparoDLC = tipo, tempoDisparoDLC = tempoNovo, donoDisparoDLC = dono })
                else Right [] -- o objeto é eliminado
          Nothing -> let novaPos = if estaEmAgua pos mapa || not (estaNoSolo pos mapa minhocas)
                            then movePosicao Sul pos
                            else pos


                     in if ePosicaoMatrizValida novaPos mapa
                      then if existeDonoMinhoca pos dono minhocas
                        then Left (DisparoDLC { posicaoDisparoDLC = novaPos, direcaoDisparoDLC = Norte, tipoDisparoDLC = tipo, tempoDisparoDLC = Just 2, donoDisparoDLC = dono })
                        else Left (DisparoDLC { posicaoDisparoDLC = novaPos, direcaoDisparoDLC = Norte, tipoDisparoDLC = tipo, tempoDisparoDLC = Nothing, donoDisparoDLC = dono })
                      else Right []


      DinamiteDLC ->
        case tempo of
          Just 0 -> Right (calculaExplosao pos 7)
          Just _ ->
            let tempoNovo = case tempo of Just t  -> Just (t - 1)
            
            in if estaNoSolo pos mapa minhocas
              then 
                -- Se está no solo, mantém a posição
                Left (DisparoDLC { posicaoDisparoDLC = pos, direcaoDisparoDLC = dir, tipoDisparoDLC = tipo, tempoDisparoDLC = tempoNovo, donoDisparoDLC = dono })
              else
                -- Se não está no solo, aplica movimento
                let (novaPos, novaDir) = case dir of
                      Norte -> (movePosicao Sul pos, Norte)
                      Sul   -> (movePosicao Sul pos, Norte)
                      _     -> rodaPosicaoDirecao (pos, dir)
                in if ePosicaoMatrizValida novaPos mapa
                  then if ePosicaoMapaLivre novaPos mapa
                    then Left (DisparoDLC { posicaoDisparoDLC = novaPos, direcaoDisparoDLC = novaDir, tipoDisparoDLC = tipo, tempoDisparoDLC = tempoNovo, donoDisparoDLC = dono })
                    else Left (DisparoDLC { posicaoDisparoDLC = movePosicao Sul pos, direcaoDisparoDLC = Norte, tipoDisparoDLC = tipo, tempoDisparoDLC = tempoNovo, donoDisparoDLC = dono })
                  else Right [] --objeto desaparece


  where
    mapa = mapaEstadoDLC e
    minhocas = minhocasEstadoDLC e


      -- todo podera ser otimizado com a funcao auxiliar da tarefa0

    -- A função verifica se há alguma minhoca inimiga à volta de uma posição (sem ser a que lançou a mina)
    existeDonoMinhoca :: Posicao -> Int -> [MinhocaDLC] -> Bool
    existeDonoMinhoca pos dono minhs =
      let

        posicoes = pos : [movePosicao Norte pos, movePosicao Sul pos, movePosicao Este pos, movePosicao Oeste pos]
        minhocasComIndice = zip [0..] minhs


        existeMinhocaInimiga :: Posicao -> [(Int, MinhocaDLC)] -> Bool
        existeMinhocaInimiga _ [] = False
        existeMinhocaInimiga p ((indice, m):ms) =
          case posicaoMinhocaDLC m of
            Just p' | p == p' && indice /= dono -> True
            _ -> existeMinhocaInimiga p ms


        verificaTodasPosicoes :: [Posicao] -> [(Int, MinhocaDLC)] -> Bool
        verificaTodasPosicoes [] _ = False
        verificaTodasPosicoes (p:ps) ms =
          (existeMinhocaInimiga p ms || verificaTodasPosicoes ps ms)

      in verificaTodasPosicoes posicoes minhocasComIndice



    estaEmAgua :: Posicao -> MapaDLC -> Bool
    estaEmAgua _ [] = False
    estaEmAgua pos m = case encontraPosicaoMatriz (movePosicao Sul pos) m of
        Nothing -> False
        Just AguaDLC -> True
        Just _ -> False

    estaEmLava :: Posicao -> MapaDLC -> Bool
    estaEmLava _ [] = False
    estaEmLava pos m = case encontraPosicaoMatriz (movePosicao Sul pos) m of
        Nothing -> False
        Just Lava -> True
        Just _ -> False



-- todo -> Isto definitivamente não é a forma mais otimizada de fazer isto, mas devido ao tempo é melhor usar assim por enquanto
-- todo -> Atualmente so se usa explosoes 3,5 e 7 logo isto serve, mas no futuro é importante transformar numa função generalizada
calculaExplosao :: Posicao -> Int -> Danos
calculaExplosao pos d = case d of
  1 -> [
          (pos, d*10)]
  3 -> [
          (pos, d*10),
          (movePosicao Norte pos, (d-2)*10),(movePosicao Sul pos, (d-2)*10),(movePosicao Oeste pos, (d-2)*10),(movePosicao Este pos, (d-2)*10)
      ]

  5 -> [
          (pos, d*10),
          (movePosicao Norte pos, (d-2)*10),(movePosicao Sul pos, (d-2)*10),(movePosicao Oeste pos, (d-2)*10),(movePosicao Este pos, (d-2)*10),
          (movePosicao Sudeste pos, (d-3)*10),(movePosicao Sudoeste pos, (d-3)*10),(movePosicao Nordeste pos, (d-3)*10),(movePosicao Noroeste pos, (d-3)*10),
          (movePosicao Norte (movePosicao Norte pos), (d-4)*10),(movePosicao Sul (movePosicao Sul pos), (d-4)*10),(movePosicao Este (movePosicao Este pos), (d-4)*10),(movePosicao Oeste (movePosicao Oeste pos), (d-4)*10)
      ]

  7 -> [ (pos, 70), (movePosicao Norte pos, 50), (movePosicao Sul pos, 50), (movePosicao Este pos, 50), (movePosicao Oeste pos, 50), (movePosicao Nordeste pos, 40), (movePosicao Noroeste pos, 40), (movePosicao Sudeste pos, 40), (movePosicao Sudoeste pos, 40), (movePosicao Norte (movePosicao Nordeste pos), 40), (movePosicao Norte (movePosicao Noroeste pos), 40), (movePosicao Sul (movePosicao Sudeste pos), 40), (movePosicao Sul (movePosicao Sudoeste pos), 40), (movePosicao Este (movePosicao Nordeste pos), 40), (movePosicao Oeste (movePosicao Noroeste pos), 40), (movePosicao Este (movePosicao Sudeste pos), 40), (movePosicao Oeste (movePosicao Sudoeste pos), 40), (movePosicao Norte (movePosicao Norte pos), 30), (movePosicao Sul (movePosicao Sul pos), 30), (movePosicao Este (movePosicao Este pos), 30), (movePosicao Oeste (movePosicao Oeste pos), 30), (movePosicao Nordeste (movePosicao Nordeste pos), 20), (movePosicao Noroeste (movePosicao Noroeste pos), 20), (movePosicao Sudeste (movePosicao Sudeste pos), 20), (movePosicao Sudoeste (movePosicao Sudoeste pos), 20), (movePosicao Norte (movePosicao Norte (movePosicao Norte pos)), 50), (movePosicao Sul (movePosicao Sul (movePosicao Sul pos)), 50), (movePosicao Este (movePosicao Este (movePosicao Este pos)), 50), (movePosicao Oeste (movePosicao Oeste (movePosicao Oeste pos)), 50) ]



{- | Para uma lista de posições afetadas por uma explosão, recebe um estado e calcula o novo estado em que esses danos são aplicados.

Funcionamento Geral:

* Atualiza os componentes de cada 'Minhoca'
* Atualiza os componentes de cada 'Objeto'
* Cria um novo 'Estado' contendo as minhocas e os objetos atualizados de acordo com o dano recebido.


==__'Terreno':__
* Atualiza o 'Mapa' do 'Estado', destruindo os terrenos destrutíveis nas posições afetadas

* Se o terreno na posição afetada for destrutível, destrói-o (substitui por 'Ar')
* Caso contrário, mantém o terreno como está


==__'Minhoca':__

* Atualiza cada 'Minhoca' na lista de 'Minhocas' do 'Estado', aplicando o dano correspondente se a 'Minhoca' estiver na posição afetada
* Se a 'Minhoca' estiver na posição afetada, reduz a sua 'Vida' pelo valor do dano
* Se a 'Vida' da 'Minhoca' for menor ou igual a zero, define a 'Vida' como 'MortaDLC'


==__'Objeto':__

* Atualiza cada 'Objeto' na lista de objetos do 'Estado'
* Se o 'Objeto' for um 'BarrilDLC' não explodido e estiver na posição afetada, marca-o como explodido
* Caso contrário, mantém o 'Objeto' como está
* Se o 'Objeto' for um 'DisparoDLC', mantém-no como está

-}
aplicaDanos :: Danos -> EstadoDLC -> EstadoDLC
aplicaDanos danos estado = estado {
    minhocasEstadoDLC = atualizaMinhocas (minhocasEstadoDLC estado),
    objetosEstadoDLC = atualizaObjetos danos (objetosEstadoDLC estado),
    mapaEstadoDLC = atualizaMapa (mapaEstadoDLC estado) danos
  }
  where


    -- * Terrenos

    atualizaMapa :: MapaDLC -> Danos -> MapaDLC
    atualizaMapa mapa [] = mapa
    atualizaMapa mapa ((pos, _):ds) =
      let
        terrenoAtual = encontraPosicaoMatriz pos mapa
        mapa' = case terrenoAtual of
          Just t -> if eTerrenoDestrutivel t
                      then destroiPosicao pos mapa
                      else mapa
          Nothing -> mapa
      in atualizaMapa mapa' ds


    -- * Minhocas

    atualizaMinhocas :: [MinhocaDLC] -> [MinhocaDLC]
    atualizaMinhocas [] = []
    atualizaMinhocas (h:t) = atualizaMinhoca h : atualizaMinhocas t


    atualizaMinhoca :: MinhocaDLC -> MinhocaDLC
    atualizaMinhoca minhoca =
      case posicaoMinhocaDLC minhoca of
        Nothing -> minhoca
        Just pos ->
          case vidaMinhocaDLC minhoca of
            MortaDLC -> minhoca
            VivaDLC v ->
              let dano = somaDanos pos danos
                  novaVida = v - dano
              in if novaVida <= 0
                 then minhoca { vidaMinhocaDLC = MortaDLC }
                 else minhoca { vidaMinhocaDLC = VivaDLC novaVida }

    somaDanos :: Posicao -> Danos -> Int
    somaDanos _ [] = 0
    somaDanos pos ((p, d):ds) =
      if pos == p
        then d + somaDanos pos ds
        else somaDanos pos ds

    -- * Objetos

    atualizaObjetos :: Danos -> [ObjetoDLC] -> [ObjetoDLC]
    atualizaObjetos _ [] = []
    atualizaObjetos ds (obj:resto) =
      let pos = posicaoObjeto obj
          objAtualizado = case obj of
            BarrilDLC _ False ->
              if posAfetado pos ds
                then BarrilDLC pos True
                else obj
            _ -> obj
      in objAtualizado : atualizaObjetos danos resto
      where

        posAfetado :: Posicao -> Danos -> Bool
        posAfetado _ [] = False
        posAfetado p ((posDano, _):t) =
          (p == posDano) || posAfetado p t