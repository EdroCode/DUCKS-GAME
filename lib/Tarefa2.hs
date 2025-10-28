{-|
Module      : Tarefa2
Description : Efetuar jogadas.

Módulo para a realização da Tarefa 2 de LI1\/LP1 em 2025\/26.
-}
module Tarefa2 where

import Labs2025
import Foreign (moveArray)


-- | Função principal da Tarefa 2. Recebe o índice de uma minhoca na lista de minhocas, uma jogada, um estado e retorna um novo estado em que essa minhoca efetuou essa jogada.
efetuaJogada :: NumMinhoca -> Jogada -> Estado -> Estado
efetuaJogada n (Move direcao) e = undefined
efetuaJogada n (Dispara arma direcao) e = undefined

                        where
                            minhoca = encontraIndiceLista n minhocas
                            minhocas = minhocasEstado e
                            
-- Verificar Movimento
movimentoValido :: Minhoca -> Direcao -> Estado -> Bool
movimentoValido m dir est = if vidaMinhoca m == Morta 
                            then False 
                                else if ePosicaoEstadoLivre posNova est == False then False else True

                    where
                        pos = posicaoMinhoca m
                        posNova = movePosicao dir pos

