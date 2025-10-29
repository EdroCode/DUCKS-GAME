{-|
Module      : Tarefa2
Description : Efetuar jogadas.

Módulo para a realização da Tarefa 2 de LI1\/LP1 em 2025\/26.
-}
module Tarefa2 where

import Labs2025
import Tarefa0_geral
import Tarefa0_2025
import Foreign (moveArray)













-- | Função principal da Tarefa 2. Recebe o índice de uma minhoca na lista de minhocas, uma jogada, um estado e retorna um novo estado em que essa minhoca efetuou essa jogada.
efetuaJogada :: NumMinhoca -> Jogada -> Estado -> Estado
efetuaJogada n (Move direcao) e = undefined
efetuaJogada n (Dispara arma direcao) e = if movimentoValido minhoca direcao e 
                                            then e -- * Ela move se
                                                
                                            else e -- * Retorna o estado anterior

                        where
                            minhoca = case encontraIndiceLista n minhocas of Just m -> m -- ! atencao
                            minhocas = minhocasEstado e



               
                            
-- * --------------------------------------
-- * MOVIMENTO
-- * --------------------------------------

mapaOk = [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
    ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
    ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
    ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
    ,[Terra,Terra,Terra,Terra,Terra,Ar,Ar,Ar,Ar,Ar]
    ,[Terra,Terra,Terra,Terra,Terra,Pedra,Pedra,Agua,Agua,Agua]
    ]

-- Verificar Movimento
movimentoValido :: Minhoca -> Direcao -> Estado -> Bool
movimentoValido m dir est = if vidaMinhoca m == Morta 
                            then False 
                                else if ePosicaoEstadoLivre posNova est == False then False else True

                    where
                        pos = case posicaoMinhoca m of Just a -> a
                        posNova = movePosicao dir pos

estaNoSolo :: Posicao -> Mapa -> Bool
estaNoSolo pos mapa = if eTerrenoOpaco blocoInferior then True else False
            where
                blocoInferior = case encontraPosicaoMatriz (movePosicao Sul pos) mapa of Just a -> a






