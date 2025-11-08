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

type Dano = Int
type Danos = [(Posicao,Dano)]

-- | Função principal da Tarefa 3. Avanço o estado do jogo um tick.
avancaEstado :: Estado -> Estado
avancaEstado e@(Estado mapa objetos minhocas) = foldr aplicaDanos e' danoss
    where
    minhocas' = map (uncurry $ avancaMinhoca e) (zip [0..] minhocas)
    (objetos',danoss) = partitionEithers $ map (uncurry $ avancaObjeto $ e { minhocasEstado = minhocas' }) (zip [0..] objetos)
    e' = Estado mapa objetos' minhocas'










-- | Para um dado estado, dado o índice de uma minhoca na lista de minhocas e o estado dessa minhoca, retorna o novo estado da minhoca no próximo tick.
avancaMinhoca :: Estado -> NumMinhoca -> Minhoca -> Minhoca
avancaMinhoca e i minhoca = case encontraPosicaoMatriz posicaoFinal mapa of
                                Nothing ->  minhoca{posicaoMinhoca = Nothing, vidaMinhoca = Morta}
                                Just Agua -> minhoca{posicaoMinhoca = Just posicaoFinal, vidaMinhoca = Morta}
                                Just Ar -> minhoca{posicaoMinhoca = Just posicaoFinal}
                                _ -> minhoca{posicaoMinhoca = Just posicaoFinal}
                               
                    where

                        -- Desdobrar minhoca
                        pos = case posicaoMinhoca minhoca of Just a -> a
                        posicaoFinal = if estaNoSolo pos mapa 
                            then pos
                            else movePosicao Sul pos
                        

                        -- Desdobrar estado
                        mapa = mapaEstado e
                        objetos = objetosEstado e
                        minhocas = minhocasEstado e






-- | Para um dado estado, dado o índice de um objeto na lista de objetos e o estado desse objeto, retorna o novo estado do objeto no próximo tick ou, caso o objeto expluda, uma lista de posições afetadas com o dano associado.
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
            if not (estaNoSolo pos mapa) || estaEmAgua pos mapa
              then
                if dir == Sul
                  then Left (Disparo { posicaoDisparo = movePosicao dir pos, direcaoDisparo = Norte, tipoDisparo = tipo, tempoDisparo = tempo, donoDisparo = dono })
                  else Left o
              else Left (Disparo { posicaoDisparo = movePosicao dir pos, direcaoDisparo = Norte, tipoDisparo = tipo, tempoDisparo = tempo, donoDisparo = dono })
          Nothing -> if not (existeDonoMinhoca pos dono minhocas) -- * Da true se nao existe nenhnuma minhoca alem do dono na posicao
                      then Left o
                      else Left (Disparo { posicaoDisparo = movePosicao dir pos, direcaoDisparo = Norte, tipoDisparo = tipo, tempoDisparo = Just 2, donoDisparo = dono })
      
      
      Dinamite ->
        case tempo of -- dinamite não tem tempo nulo
          Just 0 -> Right (calculaExplosao pos 7)
          Just _ ->
            
            if not (estaNoSolo pos mapa) -- esta no ar
              then
                let
                  
                  (x,y) = pos
                    
                  (novaPos, novaDir) = case dir of
                        Norte -> ((x, y - 1), Norte)      -- cai vertical, aponta para Norte
                        Sul   -> ((x, y - 1), Norte)    
                        _     -> rodaPosicaoDirecao (pos, dir) -- move e roda 45° para a direita
                  
                in Left (Disparo { posicaoDisparo = novaPos, direcaoDisparo = novaDir, tipoDisparo = tipo, tempoDisparo = tempoNovo, donoDisparo = dono })
              else Left (Disparo { posicaoDisparo = pos, direcaoDisparo = Norte, tipoDisparo = tipo, tempoDisparo = tempoNovo, donoDisparo = dono })
            where tempoNovo = case tempo of Just t  -> Just (t - 1)

  -- todos os right precisam ser completos, falta definir a funcao que calcule a area das explosoes e a lista de danos para conseguir usar isso como output


  where
    mapa = mapaEstado e
    minhocas = minhocasEstado e



      -- todo podera ser otimizado com a funcao auxiliar da tarefa0
      -- ! rever talvez
    existeDonoMinhoca :: Posicao -> Int -> [Minhoca] -> Bool
    existeDonoMinhoca pos n [] = False
    existeDonoMinhoca (x,y) n (h:t) = let pos = posicaoMinhoca h
                                    in if pos == Just (x,y) && n /= 0 then True else existeMinhoca (x,y) t
    

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
        
      7 -> [
            (pos, 7*10),
            (movePosicao Norte pos, 50),(movePosicao Sul pos, 50),(movePosicao Oeste pos, 50),(movePosicao Este pos, 50),
            (movePosicao Noroeste pos, 40),(movePosicao Nordeste pos, 40),(movePosicao Sudeste pos, 40),(movePosicao Sudoeste pos, 40),
            (movePosicao Norte (movePosicao Norte pos), 30),(movePosicao Sul (movePosicao Sul pos), 30),(movePosicao Este (movePosicao Este pos), 30),(movePosicao Oeste (movePosicao Oeste pos), 30),
            (movePosicao Noroeste (movePosicao Noroeste pos), 20),(movePosicao Nordeste (movePosicao Nordeste pos), 20),(movePosicao Sudeste (movePosicao Sudeste pos), 20),(movePosicao Sudoeste (movePosicao Sudoeste pos), 20),
            (movePosicao Norte (movePosicao Norte (movePosicao Norte pos)), 10),(movePosicao Sul (movePosicao Sul (movePosicao Sul pos)), 10),(movePosicao Este (movePosicao Este (movePosicao Este pos)), 10),(movePosicao Oeste (movePosicao Oeste (movePosicao Oeste pos)), 10)
        ]
       

-- | Para uma lista de posições afetadas por uma explosão, recebe um estado e calcula o novo estado em que esses danos são aplicados.
aplicaDanos :: Danos -> Estado -> Estado
aplicaDanos danos estado = estado {
    minhocasEstado = atualizaMinhocas (minhocasEstado estado),
    mapaEstado = atualizaMapa (mapaEstado estado) danos
  }
  where


    -- * Terrenos

    atualizaMapa :: Mapa -> Danos -> Mapa
    atualizaMapa mapa [] = mapa
    atualizaMapa mapa ((pos, _):ds) =
      let 
        terrenoAtual = case encontraPosicaoMatriz pos mapa of Just a -> a
        mapa' = if eTerrenoDestrutivel (terrenoAtual)
                    then destroiPosicao pos mapa
                    else mapa
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

    







