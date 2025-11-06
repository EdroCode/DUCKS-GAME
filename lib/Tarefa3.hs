{-|
Module      : Tarefa3
Description : Avançar tempo do jogo.

Módulo para a realização da Tarefa 3 de LI1\/LP1 em 2025\/26.
-}

module Tarefa3 where

import Data.Either
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
    Barril pos explode
        | explode -> Right [(pos, 50)] -- Barril explode
        | if (estaNoSolo pos mapa) then Left o -- Barril ta no chao  
                else
            let newPos = movePosicao Sul pos
            in case encontraPosicaoMatriz newPos mapa of
                Just Agua -> Right [(newPos, 0)] -- Barril some na agua
                Nothing -> Right [(pos, 0)] -- Barril caiu do mapa
                otherwise -> Left (Barril newPos False) -- Barril ta no ar e cai
    
    Disparo pos dir tempo dono -> case tipoDisparo of
        Jetpack -> Right [(pos, 0)] -- Some jetpack e usa ele

        Escavadora
            Just Terra -> Right [(pos, 25)] -- Destroi terra
            _ -> Right [(pos, 0)] -- em qlqr outro terreno some
        Bazuca -> case tempo of
            Just t | t <= 0 -> Right [(pos, 75)]  -- Bazuca explode com tempo 0
            Just t -> Left (Disparo pos dir tipo (Just (t-1)) dono)  -- cronometro
            Nothing -> 
                let newPos = movePosicao dir pos
                in case encontraPosicaoMatriz newPos mapa of
                    Just Pedra -> Right [(pos, 75)]  -- Explode na pedra
                    Just Terra -> Right [(pos, 75)]  -- Explode na terra
                    Just Ar -> Left (Disparo newPos dir tipo Nothing dono)  -- Continua movimento
                    _ -> Right [(pos, 75)]  -- Explode em outros casos
        
        Mina -> case encontraPosicaoMatriz pos mapa of
            Just Terra -> Right [(pos, 40)]  -- Mina explode na terra
            Just Ar | estaNoSolo pos mapa -> Left o  -- Mina fica no chão
            Just Ar -> Left (Disparo (movePosicao Sul pos) dir tipo tempo dono)  -- Mina cai
            _ -> Right [(pos, 40)]  -- Mina explode em outros casos
        
        Dinamite -> case tempo of
            Just t | t <= 0 -> Right [(pos, 100)]  -- Dinamite explode com tempo 0
            Just t -> Left (Disparo pos dir tipo (Just (t-1)) dono)  -- cronometro
            Nothing -> Right [(pos, 100)] 
    where
        mapa = mapaEstado e


        

-- | Para uma lista de posições afetadas por uma explosão, recebe um estado e calcula o novo estado em que esses danos são aplicados.
aplicaDanos :: Danos -> Estado -> Estado
aplicaDanos ds e = undefined
