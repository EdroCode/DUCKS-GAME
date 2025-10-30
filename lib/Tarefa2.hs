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

-- * Assume se que todos os estados recebidos serao validos

estadoTeste = Estado
    { mapaEstado = mapaOk
    , objetosEstado =
        []
    , minhocasEstado =
        [Minhoca {posicaoMinhoca = Just (2,0), vidaMinhoca = Viva 100, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}
        ]
    }

jogadaTeste = Move Norte




-- | Função principal da Tarefa 2. Recebe o índice de uma minhoca na lista de minhocas, uma jogada, um estado e retorna um novo estado em que essa minhoca efetuou essa jogada.
efetuaJogada :: NumMinhoca -> Jogada -> Estado -> Estado
efetuaJogada n (Dispara arma direcao) e = undefined
efetuaJogada n (Move direcao) e = if not (vidaMinhoca minhoca == Morta)  -- * Se a posicao nao for nula (para so aceitar posicoes validas nas funcoes seguintes)
                                    then if not (posicaoMinhoca minhoca == Nothing)
                                                then if estaNoSolo pos mapa  -- * esta no solo (nao esta no ar)
                                                        then case direcao of-- * Ela move se
                                                            _ -> estadoFinal -- * Move se
                                                    else e 
                                                else e -- * esta no Ar
                                    else e
                                
                                
                        where
                            -- * minhoca estudada
                            minhoca = case encontraIndiceLista n minhocas of Just m -> m -- ! atencao
                            pos = case posicaoMinhoca minhoca of Just a -> a
                             
                            -- -------------
                            
                            minhocas = minhocasEstado e
                            mapa = mapaEstado e
                            objetos = objetosEstado e

                            novaPos = movePosicao direcao pos

                            
                            -- * Estado final

                            minhocaFinal = -- * NOva pos n é opaca
                                if ePosicaoMatrizValida pos mapa
                                    then if eNovaPosLivre pos e
                                        then minhoca { posicaoMinhoca = Just novaPos } 
                                        else minhoca
                                else minhoca { posicaoMinhoca = Nothing, vidaMinhoca = Morta }
                            
                            
                            
                            
                            
                            minhocasFinais = atualizaIndiceLista n minhocaFinal minhocas

                            estadoFinal = Estado {
                                mapaEstado = mapa,
                                objetosEstado = objetos,
                                minhocasEstado = minhocasFinais
                            } -- todo por definir

                            -- * Auxiliares

                            


               
                            
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


-- Verifica se a posicao que a minhoca ira é nao opaca
eNovaPosLivre :: Posicao -> Estado -> Bool
eNovaPosLivre posNova est = if ePosicaoEstadoLivre posNova est == False then False else True

 
-- ! not exhaustive ERROR
estaNoSolo :: Posicao -> Mapa -> Bool
estaNoSolo p [] = False
estaNoSolo pos mapa = if eTerrenoOpaco blocoInferior && not (eTerrenoOpaco blocoAtual) then True else False
            where
                blocoInferior = case encontraPosicaoMatriz (movePosicao Sul pos) mapa of Just a -> a
                blocoAtual =  case encontraPosicaoMatriz pos mapa of Just a -> a


 