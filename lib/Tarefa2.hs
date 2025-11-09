{-|
Module      : Tarefa2
Description : Efetuar jogadas.

Módulo para a realização da Tarefa 2 de LI1\/LP1 em 2025\/26.
-}
module Tarefa2 where

import Labs2025
import Tarefa0_geral
import Tarefa0_2025
import Tarefa1
import Foreign (moveArray)

-- * Efetuar Jogada


{-| Recebe o índice de uma minhoca na lista de minhocas, uma jogada, um estado e retorna um novo estado em que essa minhoca efetuou essa jogada.


| Funcionamento:

@
 Movimentos (`Move`) alteram a 'Posicao' da 'Minhoca' se a posição destino for
    válida e livre; se a minhoca estiver no 'Ar' ou 'Morta' a 'Jogada' é ignorada.
@
@
Disparos (`Dispara`) consomem munição da minhoca (quando aplicável), podem
    criar objetos do tipo `Disparo` no estado (ex.: `Bazuca`, `Mina`, `Dinamite`)
    ou modificar o mapa (ex.: `Escavadora` transforma terreno em `Ar`).
@
@
A função devolve o mesmo `Estado` sem alterações quando a jogada for
    inválida ou não aplicável (minhoca morta, posição inválida, dono já tem
    disparo do mesmo tipo, etc.).
@

==__Exemplos:__

@
estadoValido5 = Estado
    { mapaEstado =
        [[Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Pedra,Agua,Agua]
        ,[Terra,Terra,Terra,Terra,Pedra,Agua]
        ]
    , objetosEstado =[]
    , minhocasEstado =
        [Minhoca {posicaoMinhoca = Just (1,1), vidaMinhoca = Viva 100, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}]
    }
 
@

>jogada1teste = Dispara Jetpack Norte

>>>efetuaJogada 0 jogada1teste estadoValido5 = Estado {mapaEstado = [[Ar,Ar,Ar,Ar,Ar,Ar],[Ar,Ar,Ar,Ar,Ar,Ar],[Terra,Terra,Terra,Pedra,Agua,Agua],[Terra,Terra,Terra,Terra,Pedra,Agua]], objetosEstado = [], minhocasEstado = [Minhoca {posicaoMinhoca = Just (0,1), vidaMinhoca = Viva 100, jetpackMinhoca = 0, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}]}

-}


efetuaJogada :: NumMinhoca -> Jogada -> Estado -> Estado

-- DISPARO - Dispara TIpo Direcao
efetuaJogada n (Dispara arma direcao) e = if not (vidaMinhoca minhoca == Morta) -- não esta morta
                                            then if temMunicao minhoca arma
                                                then if not (elem (arma,n) (listaDonos objetos)) -- Verifica se existe um disparo da mesma arma da mesma minhoca no estado atual
                                                    then case arma of

                                                        Jetpack -> if ePosicaoMatrizValida novaPos mapa && ePosicaoEstadoLivre novaPos e
                                                                then Estado {mapaEstado = mapa,objetosEstado = objetos,minhocasEstado = minhocasFinais}
                                                                else e{minhocasEstado = minhocasFinais}
                                                        
                                                        Escavadora -> if ePosicaoMatrizValida novaPos mapa
                                                                then if not (blocoTargeted == Pedra)
                                                                    then if eTerrenoDestrutivel(blocoTargeted)
                                                                        then Estado {mapaEstado = atualizaPosicaoMatriz novaPos Ar mapa ,objetosEstado = objetos,minhocasEstado = minhocasFinais}
                                                                        else Estado {mapaEstado = mapa ,objetosEstado = objetos,minhocasEstado = minhocasFinais}
                                                                    
                                                                    else Estado{mapaEstado=mapa,objetosEstado=objetos,minhocasEstado=atualizaIndiceLista n (minhoca{escavadoraMinhoca = escavadoraMinhoca minhoca-1}) minhocas}                                                
                                                                else e{minhocasEstado = minhocasFinais}

                                                        Bazuca -> if ePosicaoMatrizValida novaPos mapa

                                                                then Estado{mapaEstado=mapaEstado e,objetosEstado = (objetosEstado e ++ [Disparo{posicaoDisparo=novaPos, direcaoDisparo=direcao, tipoDisparo=Bazuca, tempoDisparo=Nothing, donoDisparo=n}]), minhocasEstado = minhocasFinais}
                                                                else e{minhocasEstado = minhocasFinais} 

                                                        Mina -> if ePosicaoMatrizValida novaPos mapa
                                                                then if ePosicaoMapaLivre novaPos mapa
                                                                    then Estado{mapaEstado=mapaEstado e,objetosEstado = (objetosEstado e ++ [Disparo{posicaoDisparo=novaPos, direcaoDisparo=direcao, tipoDisparo=Mina, tempoDisparo=Nothing, donoDisparo=n}]), minhocasEstado = minhocasFinais}
                                                                    else Estado{mapaEstado=mapaEstado e,objetosEstado = (objetosEstado e ++ [Disparo{posicaoDisparo=pos, direcaoDisparo=direcao, tipoDisparo=Mina, tempoDisparo=Nothing, donoDisparo=n}]), minhocasEstado = minhocasFinais}

                                                                     
                                                                else e{minhocasEstado = minhocasFinais} 
                                                        
                                                        Dinamite -> if ePosicaoMatrizValida novaPos mapa
                                                                then if ePosicaoMapaLivre novaPos mapa
                                                                    then Estado{mapaEstado=mapaEstado e,objetosEstado = (objetosEstado e ++ [Disparo{posicaoDisparo=novaPos, direcaoDisparo=direcao, tipoDisparo=Dinamite, tempoDisparo=Just 4, donoDisparo=n}]), minhocasEstado = minhocasFinais}
                                                                    else Estado{mapaEstado=mapaEstado e,objetosEstado = (objetosEstado e ++ [Disparo{posicaoDisparo=pos, direcaoDisparo=direcao, tipoDisparo=Dinamite, tempoDisparo=Just 4, donoDisparo=n}]), minhocasEstado = minhocasFinais}
                                                                     
                                                                else e{minhocasEstado = minhocasFinais} 
                                          
                                                    else e
                                                else e
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

                                            minhocaFinal =
                                                let
                                                    bloco =
                                                        if ePosicaoMatrizValida novaPos mapa
                                                            then encontraPosicaoMatriz novaPos mapa
                                                            else Nothing

                                                    atualizaMunicao m = case arma of
                                                        Jetpack    -> m { jetpackMinhoca    = jetpackMinhoca m - 1 }
                                                        Escavadora -> m { escavadoraMinhoca = escavadoraMinhoca m - 1 }
                                                        Bazuca     -> m { bazucaMinhoca     = bazucaMinhoca m - 1 }
                                                        Mina       -> m { minaMinhoca       = minaMinhoca m - 1 }
                                                        Dinamite   -> m { dinamiteMinhoca   = dinamiteMinhoca m - 1 }

                                                    (novaPosicao, novaVida) =
                                                        if not (ePosicaoMatrizValida novaPos mapa) then
                                                            case arma of
                                                                Jetpack    -> (Nothing, Morta)  -- jetpack fora do mapa -> morre
                                                                Escavadora -> (posicaoMinhoca minhoca, vidaMinhoca minhoca)  -- escavadora n mexe
                                                                _          -> (posicaoMinhoca minhoca, vidaMinhoca minhoca)  -- outras arma fica parada
                                                        else case bloco of
                                                            Just Agua ->
                                                                case arma of
                                                                    Jetpack    -> (Just novaPos, Morta)
                                                                    Escavadora -> (Just novaPos, Morta)
                                                                    _          -> (posicaoMinhoca minhoca, vidaMinhoca minhoca)
                                                            _ ->
                                                                case arma of
                                                                    Jetpack    -> (Just novaPos, vidaMinhoca minhoca)
                                                                    Escavadora -> (Just novaPos, vidaMinhoca minhoca)
                                                                    _          -> (posicaoMinhoca minhoca, vidaMinhoca minhoca)

                                                in atualizaMunicao minhoca
                                                    { posicaoMinhoca = novaPosicao
                                                    , vidaMinhoca    = novaVida
                                                    }



                                            temMunicao :: Minhoca -> TipoArma -> Bool
                                            temMunicao m arma = case arma of
                                                Jetpack -> jetpackMinhoca m > 0
                                                Escavadora -> jetpackMinhoca m > 0
                                                Bazuca -> jetpackMinhoca m > 0
                                                Mina -> jetpackMinhoca m > 0
                                                Dinamite -> jetpackMinhoca m > 0
                                           


                                            blocoTargeted = case encontraPosicaoMatriz novaPos mapa of Just a -> a
                                        
                                            minhocasFinais = atualizaIndiceLista n minhocaFinal minhocas

                       


-- MOVIMENTO
efetuaJogada n (Move direcao) e = if not (vidaMinhoca minhoca == Morta)  -- * Se a posicao nao for nula (para so aceitar posicoes validas nas funcoes seguintes)
                                    then if not (posicaoMinhoca minhoca == Nothing)
                                                then if estaNoSolo pos mapa  -- * esta no solo (nao esta no ar)
                                                        then estadoFinal -- * Move se
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
                                if ePosicaoMatrizValida novaPos mapa
                                    then if ePosicaoEstadoLivre novaPos e
                                        then if not ((encontraPosicaoMatriz novaPos mapa) == Just Agua) 
                                            then minhoca { posicaoMinhoca = Just novaPos } 
                                            else minhoca { posicaoMinhoca = Just novaPos,  vidaMinhoca = Morta } 
                                        else minhoca
                                else minhoca { posicaoMinhoca = Nothing, vidaMinhoca = Morta }
                            
                            
                            
                            minhocasFinais = atualizaIndiceLista n minhocaFinal minhocas

                            estadoFinal = Estado {
                                mapaEstado = mapa,
                                objetosEstado = objetos,
                                minhocasEstado = minhocasFinais
                            } 


-- removi a função (e novaposlivre) porque era estupida e eu deveria tar maluco se fui eu q escrevi aquilo      
 


 -- todo mudar para estaNoAr e a logica da mesma para melhor readability?
{-|Verifica se uma posição está apoiada em terreno opaco. Retorna True quando a posição existe no mapa, o bloco abaixo é opaco e o bloco atual não é opaco. Retorna False em casos onde a posição está fora do mapa ou quando a condição não se verifica.

Funcionamento:

* Retorna 'False' se a posição está fora do mapa
* Retorna 'True' se o bloco abaixo da posição for opaco e o bloco atual não for opaco
* Retorna 'False' caso contrário

==__Exemplos:__

@
mapaOk = 
    [ [Ar,Ar,Ar,Ar,Ar]
    , [Ar,Ar,Ar,Ar,Ar]
    , [Terra,Terra,Terra,Pedra,Agua]
    , [Terra,Terra,Terra,Terra,Pedra]
    ]
@

>>> estaNoSolo (1,2) mapaOk
True

>>> estaNoSolo (2,3) mapaOk
False

-}
estaNoSolo :: Posicao -> Mapa -> Bool
estaNoSolo p [] = False
estaNoSolo pos mapa = case encontraPosicaoMatriz (movePosicao Sul pos) mapa of
    Nothing -> False
    Just blocoInferior -> case encontraPosicaoMatriz pos mapa of
        Nothing -> False
        Just blocoAtual -> eTerrenoOpaco blocoInferior && not (eTerrenoOpaco blocoAtual)

{-|Verifica se a posição tem água imediatamente abaixo. Retorna True quando o bloco imediatamente abaixo da posição é Agua. Retorna False caso contrário.

Funcionamento:

* Retorna 'False' se a posição está fora do mapa
* Retorna 'True' se o bloco imediatamente abaixo da posição for Agua
* Retorna 'False' caso contrário

==__Exemplos:__
@
mapaOk = 
    [ [Ar,Ar,Ar,Ar,Ar]
    , [Ar,Ar,Ar,Ar,Ar]
    , [Terra,Terra,Terra,Agua,Agua]
    , [Terra,Terra,Terra,Agua,Agua]
    ]
@

>>> estaEmAgua (3,2) mapaOk
False

>>> estaEmAgua (2,3) mapaOk
True

-}
estaEmAgua :: Posicao -> Mapa -> Bool
estaEmAgua p [] = False
estaEmAgua pos mapa = case encontraPosicaoMatriz (movePosicao Sul pos) mapa of
    Nothing -> False
    Just Agua -> True
    Just _ -> False
