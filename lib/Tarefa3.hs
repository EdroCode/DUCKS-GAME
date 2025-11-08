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







barrilTestador = Barril{posicaoBarril = (2,1), explodeBarril = False}
disparoTestador = Disparo{posicaoDisparo = (2,0), direcaoDisparo = Sudeste, tipoDisparo = Dinamite, tempoDisparo = Just 2, donoDisparo = 0}
minhocaValida1 = Minhoca{posicaoMinhoca=Just (2,0), vidaMinhoca=Morta, jetpackMinhoca=100, escavadoraMinhoca=200, bazucaMinhoca=150, minaMinhoca=3, dinamiteMinhoca=1} -- posição válida, morta, munições >= 0


teste = Estado
    { mapaEstado =
        [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Agua,Agua]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Agua,Agua]
        ]
    , objetosEstado = [barrilTestador, disparoTestador]
    , minhocasEstado = [minhocaValida1]
        
    } 



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
          else Left o -- todo
      else Left o --todo n é o

  Disparo pos dir tipo tempo dono -> case tipo of
      Bazuca ->
        let posNova = movePosicao dir pos
            tempoNovo = case tempo of
              Just t  -> Just (t - 1)
              Nothing -> Nothing
        in if ePosicaoMatrizValida posNova mapa
          then if ePosicaoMapaLivre pos mapa
            then Left (Disparo{posicaoDisparo = posNova, direcaoDisparo = dir, tipoDisparo = tipo, tempoDisparo = tempoNovo, donoDisparo = dono})
            else Right []         
          else Right []
        
      Mina ->
        case tempo of
          Just 0 -> Right []
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
          Just 0 -> Right []
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
    
              

-- | Para uma lista de posições afetadas por uma explosão, recebe um estado e calcula o novo estado em que esses danos são aplicados.
aplicaDanos :: Danos -> Estado -> Estado
aplicaDanos ds e = undefined
