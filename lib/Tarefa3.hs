{-|
Module      : Tarefa3
Description : Avançar tempo do jogo.

Módulo para a realização da Tarefa 3 de LI1\/LP1 em 2025\/26.
-}

module Tarefa3 where

import Data.Either
import Tarefa0_geral
import Tarefa0_2025
import Tarefa2
import Labs2025

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
avancaEstado :: Estado -> Estado
avancaEstado e@(Estado mapa objetos minhocas) = foldr aplicaDanos e' danoss
    where
    minhocas' = map (uncurry $ avancaMinhoca e) (zip [0..] minhocas)
    (objetos',danoss) = partitionEithers $ map (uncurry $ avancaObjeto $ e { minhocasEstado = minhocas' }) (zip [0..] objetos)
    e' = Estado mapa objetos' minhocas' 

-- * Funções Auxiliares

{- |Para um dado 'Estado', dado o índice('NumMinhoca') de uma 'Minhoca' na lista de minhocas e o 'Estado' dessa minhoca, retorna o novo 'Estado' da 'Minhoca' no próximo tick('Ticks').

Funcionamento:

* Verifica se a 'Minhoca' tem posição

* Verifica a 'Vida' da 'Minhoca'

* Calcula a nova posição da 'Minhoca' (cair ou mover)

* Verifica o 'Terreno' na nova 'Posicao' e atualiza a 'Minhoca' conforme o novo 'Terreno'

-}
avancaMinhoca :: Estado -> NumMinhoca -> Minhoca -> Minhoca
avancaMinhoca e i minhoca =
  case posicaoMinhoca minhoca of
    Nothing -> minhoca
    Just pos ->
      let
        posicaoTentativa = if estaNoSolo pos mapa || estaMinhocaBaixoViva pos e
                              then pos
                              else movePosicao Sul pos

        -- Se a posição tentativa for inválida, a minhoca morre e desaparece
        posicaoFinalValida = ePosicaoMatrizValida posicaoTentativa mapa
        terreno = encontraPosicaoMatriz posicaoTentativa mapa
      in if not posicaoFinalValida
            then minhoca { posicaoMinhoca = Nothing, vidaMinhoca = Morta }
            else case vidaMinhoca minhoca of
              Morta -> minhoca { posicaoMinhoca = Just posicaoTentativa, vidaMinhoca = Morta }
              Viva 0 -> case terreno of
                Just Agua -> minhoca { posicaoMinhoca = Just posicaoTentativa, vidaMinhoca = Morta }
                Just Ar   -> minhoca { posicaoMinhoca = Just posicaoTentativa, vidaMinhoca = Morta }
                _         -> minhoca { posicaoMinhoca = Just posicaoTentativa, vidaMinhoca = Morta }
              Viva _ -> case terreno of
                Just Agua -> minhoca { posicaoMinhoca = Just posicaoTentativa, vidaMinhoca = Morta }
                Just Ar   -> minhoca { posicaoMinhoca = Just posicaoTentativa }
                _         -> minhoca { posicaoMinhoca = Just posicaoTentativa }

  where
    mapa = mapaEstado e
    minhocas = minhocasEstado e

    estaMinhocaBaixoViva :: Posicao -> Estado -> Bool
    estaMinhocaBaixoViva pos e =
      let
        posAbaixo = movePosicao Sul pos
        
        getMinhocaViva :: Posicao -> [Minhoca] -> Bool
        getMinhocaViva _ [] = False
        getMinhocaViva p (h:t) =
          case posicaoMinhoca h of
            Just p' ->
              if p == p'
                then case vidaMinhoca h of
                      Viva a -> if a > 0 then True else getMinhocaViva p t
                      _      -> getMinhocaViva p t
                else getMinhocaViva p t
            Nothing -> getMinhocaViva p t

      in ePosicaoMatrizValida posAbaixo mapa
         && not (ePosicaoEstadoLivre posAbaixo e)
         && getMinhocaViva posAbaixo minhocas


{- | Para um dado 'Estado', dado o índice('NumObjeto') de um 'Objeto' na lista de objetos e o estado desse objeto, retorna o novo estado do objeto no próximo tick ou, caso o objeto exploda, uma lista de posições afetadas com o dano associado. Cada 'Objeto' possui um comportamento diferente

Funcinamento Geral:

* Recebe o 'Estado', o índice do 'Objeto' e o 'Objeto' atual

* Para cada tipo de 'Objeto', aplica a lógica específica:

==__'Barril':__

* Se o 'Barril' não está a explodir:
  * Verifica se está no solo ou em água
    * Se não estiver, marca o 'Barril' para explodir
    * Caso contrário, mantém o 'Barril' inalterado

==__'Bazuca':__
* Move o 'Disparo' na direção especificada
* Verifica se a nova posição é válida e livre
  * Se for, atualiza a posição do 'Disparo'
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

avancaObjeto :: Estado -> NumObjeto -> Objeto -> Either Objeto Danos
avancaObjeto e i o = case o of
  Barril posBarril explode ->
    if not explode
      then
        if not (estaNoSolo posBarril mapa) || estaEmAgua posBarril mapa -- esta no ar ou em agua
          then Left (Barril { posicaoBarril = posBarril, explodeBarril = True })
          else Left o
      else Right (calculaExplosao posBarril 5)

  Disparo pos dir tipo tempo dono -> case tipo of
      Bazuca ->
        let posNova = movePosicao dir pos
            tempoNovo = case tempo of
              Just t  -> Just (t - 1)
              Nothing -> Nothing
        in if ePosicaoEstadoLivre pos e
          then if ePosicaoMatrizValida posNova mapa
            then Left (Disparo{posicaoDisparo = posNova, direcaoDisparo = dir, tipoDisparo = tipo, tempoDisparo = tempoNovo, donoDisparo = dono})
            else Right (calculaExplosao pos 5)     
          else Right (calculaExplosao pos 5)
        
      Mina ->
        case tempo of
          Just 0 -> Right (calculaExplosao pos 3)
          Just _ ->
            let
              tempoNovo = case tempo of
                Just t  -> Just (t - 1)
                Nothing -> Nothing
            in if not (estaNoSolo pos mapa) || estaEmAgua pos mapa
              then
                if dir == Sul
                  then Left (Disparo { posicaoDisparo = movePosicao dir pos, direcaoDisparo = Norte, tipoDisparo = tipo, tempoDisparo = tempoNovo, donoDisparo = dono })
                  else Left (Disparo { posicaoDisparo = movePosicao Sul pos, direcaoDisparo = Norte, tipoDisparo = tipo, tempoDisparo = tempoNovo, donoDisparo = dono })
              else Left (Disparo { posicaoDisparo = pos, direcaoDisparo = Norte, tipoDisparo = tipo, tempoDisparo = tempoNovo, donoDisparo = dono })
          Nothing -> if not (existeDonoMinhoca pos dono minhocas) 
                      then if not (estaNoSolo pos mapa) || estaEmAgua pos mapa
                        then Left (Disparo { posicaoDisparo = movePosicao Sul pos, direcaoDisparo = Norte, tipoDisparo = tipo, tempoDisparo = Nothing, donoDisparo = dono })
                        else Left (Disparo { posicaoDisparo = pos, direcaoDisparo = Norte, tipoDisparo = tipo, tempoDisparo = Nothing, donoDisparo = dono })
                      else if not (estaNoSolo pos mapa) || estaEmAgua pos mapa
                        then Left (Disparo { posicaoDisparo = movePosicao Sul pos, direcaoDisparo = Norte, tipoDisparo = tipo, tempoDisparo = Nothing, donoDisparo = dono })
                        else Left (Disparo { posicaoDisparo = pos, direcaoDisparo = Norte, tipoDisparo = tipo, tempoDisparo = Just 2, donoDisparo = dono })
      
                        
                        
                        
      
      Dinamite ->
        case tempo of -- dinamite não tem tempo nulo
          Just 0 -> Right (calculaExplosao pos 7)
          Just _ ->
            if not (estaNoSolo pos mapa) -- esta no ar
              then
                let  
                  (novaPos, novaDir) = case dir of
                        Norte -> (movePosicao Sul pos, Norte)      -- cai vertical, aponta para Norte
                        Sul   -> (movePosicao Sul pos, Norte)    
                        _     -> rodaPosicaoDirecao (pos, dir) -- move e roda 45° para a direita
                  
                in Left (Disparo { posicaoDisparo = novaPos, direcaoDisparo = novaDir, tipoDisparo = tipo, tempoDisparo = tempoNovo, donoDisparo = dono })
              else let  
                  (novaPos, novaDir) = case dir of
                        Norte -> (pos, Norte)     
                        Sul   -> (pos, Norte)    
                        _     -> rodaPosicaoDirecao (pos, dir)
                  
                in Left (Disparo { posicaoDisparo = novaPos, direcaoDisparo = novaDir, tipoDisparo = tipo, tempoDisparo = tempoNovo, donoDisparo = dono })
            where tempoNovo = case tempo of Just t  -> Just (t - 1)

  -- todos os right precisam ser completos, falta definir a funcao que calcule a area das explosoes e a lista de danos para conseguir usar isso como output


  where
    mapa = mapaEstado e
    minhocas = minhocasEstado e



      -- todo podera ser otimizado com a funcao auxiliar da tarefa0
      -- ! rever talvez
    existeDonoMinhoca :: Posicao -> Int -> [Minhoca] -> Bool
    existeDonoMinhoca pos dono minhocas =
      let
        
        posicoes = pos : [movePosicao Norte pos, movePosicao Sul pos, movePosicao Este pos, movePosicao Oeste pos]

        minhocasComIndice = zip [0..] minhocas

       
        existeMinhocaInimiga :: Posicao -> [(Int, Minhoca)] -> Bool
        existeMinhocaInimiga _ [] = False
        existeMinhocaInimiga p ((i, m):ms) =
          case posicaoMinhoca m of
            Just p' | p == p' && i /= dono -> True
            _ -> existeMinhocaInimiga p ms

        
        verificaTodasPosicoes :: [Posicao] -> [(Int, Minhoca)] -> Bool
        verificaTodasPosicoes [] _ = False
        verificaTodasPosicoes (p:ps) minhocas =
          if existeMinhocaInimiga p minhocas
            then True
            else verificaTodasPosicoes ps minhocas

      in verificaTodasPosicoes posicoes minhocasComIndice



    -- todo -> Isto definitivamente não é a forma mais otimizada de fazer isto, mas devido ao tempo é melhor usar assim por enquanto
    -- todo -> Atualmente so se usa explosoes 3,5 e 7 logo isto serve, mas no futuro é importante transformar numa função generalizada
    calculaExplosao :: Posicao -> Int -> Danos 
    calculaExplosao pos d = case d of
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
* Se a 'Vida' da 'Minhoca' for menor ou igual a zero, define a 'Vida' como 'Morta'


==__'Objeto':__

* Atualiza cada 'Objeto' na lista de objetos do 'Estado'
* Se o 'Objeto' for um 'Barril' não explodido e estiver na posição afetada, marca-o como explodido
* Caso contrário, mantém o 'Objeto' como está
* Se o 'Objeto' for um 'Disparo', mantém-no como está

-}
aplicaDanos :: Danos -> Estado -> Estado
aplicaDanos danos estado = estado {
    minhocasEstado = atualizaMinhocas (minhocasEstado estado),
    objetosEstado = atualizaObjetos danos (objetosEstado estado),
    mapaEstado = atualizaMapa (mapaEstado estado) danos
  }
  where


    -- * Terrenos

    atualizaMapa :: Mapa -> Danos -> Mapa
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
    
    atualizaMinhocas :: [Minhoca] -> [Minhoca]
    atualizaMinhocas [] = []
    atualizaMinhocas (h:t) = atualizaMinhoca h : atualizaMinhocas t

    
    atualizaMinhoca :: Minhoca -> Minhoca
    atualizaMinhoca minhoca =
      case posicaoMinhoca minhoca of
        Nothing -> minhoca
        Just pos ->
          case vidaMinhoca minhoca of
            Morta -> minhoca
            Viva v ->
              let dano = somaDanos pos danos
                  novaVida = v - dano
              in if novaVida <= 0
                 then minhoca { vidaMinhoca = Morta }
                 else minhoca { vidaMinhoca = Viva novaVida }

    somaDanos :: Posicao -> Danos -> Int
    somaDanos _ [] = 0
    somaDanos pos ((p, d):ds) =
      if pos == p
        then d + somaDanos pos ds
        else somaDanos pos ds

-- * Objetos
    
    atualizaObjetos :: Danos -> [Objeto] -> [Objeto]
    atualizaObjetos _ [] = []
    atualizaObjetos danos (obj:resto) =
      let pos = posicaoObjeto obj
          objAtualizado = case obj of
            Barril _ False ->
              if posAfetado pos danos
                then Barril pos True
                else obj
            _ -> obj
      in objAtualizado : atualizaObjetos danos resto
      where

        posAfetado :: Posicao -> Danos -> Bool
        posAfetado _ [] = False
        posAfetado p ((posDano, _):t) =
          if p == posDano then True else posAfetado p t

    







