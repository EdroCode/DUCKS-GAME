{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use /=" #-}
{-# HLINT ignore "Use isJust" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-|
Module      : EfetuaJogada
Description : Efetuar jogadas.

Variante da Tarefa2
-}


module EfetuaJogada where


import Auxiliar
import Labs2025(NumMinhoca, Posicao,Direcao(Sul))
import DataDLC


{-| Recebe o índice de uma minhoca na lista de minhocas, uma jogada, um estado e retorna um novo estado em que essa minhoca efetuou essa jogada.


| Funcionamento:

@
 Movimentos (`Move`) alteram a 'Posicao' da 'Minhoca' se a posição destino for
    válida e livre; se a minhoca estiver no 'Ar' ou 'MortaDLC' a 'Jogada' é ignorada.
@
@
Disparos (`Dispara`) consomem munição da minhoca (quando aplicável), podem
    criar objetos do tipo `Disparo` no estado (ex.: `BazucaDLC`, `MinaDLC`, `DinamiteDLC`)
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
    { mapaEstadoDLC =
        [[Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Pedra,Agua,Agua]
        ,[Terra,Terra,Terra,Terra,Pedra,Agua]
        ]
    , objetosEstadoDLC =[]
    , minhocasEstadoDLC =
        [Minhoca {posicaoMinhoca = Just (1,1), vidaMinhoca = VivaDLC 100, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}]
    }
 
@

>jogada1teste = Dispara Jetpack Norte

>>>efetuaJogada 0 jogada1teste estadoValido5 = Estado {mapaEstadoDLC = [[Ar,Ar,Ar,Ar,Ar,Ar],[Ar,Ar,Ar,Ar,Ar,Ar],[Terra,Terra,Terra,Pedra,Agua,Agua],[Terra,Terra,Terra,Terra,Pedra,Agua]], objetosEstadoDLC = [], minhocasEstadoDLC = [Minhoca {posicaoMinhoca = Just (0,1), vidaMinhoca = VivaDLC 100, jetpackMinhoca = 0, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}]}

-}



efetuaJogada :: NumMinhoca -> JogadaDLC -> EstadoDLC -> EstadoDLC
-- DISPARO - Dispara TIpo Direcao
efetuaJogada n (Dispara arma direcao) e = if indiceValido n e && vidaMinhocaDLC minhoca /= MortaDLC && temMunicao minhoca arma && notElem (arma,n) (Auxiliar.listaDonos objetos) -- Verifica se existe um disparo da mesma arma da mesma minhoca no estado atual
                                                    then case arma of

                                                        JetpackDLC -> if ePosicaoMatrizValida novaPos mapa && ePosicaoEstadoLivre novaPos e
                                                                then EstadoDLC {mapaEstadoDLC = mapa,objetosEstadoDLC = objetos,minhocasEstadoDLC = minhocasFinais, armaSelecionada = Just arma, minhocaSelecionada = minhocaSelecionada e, danosEstado = danosEstado e}



                                                                else e{minhocasEstadoDLC = minhocasFinais}

                                                        EscavadoraDLC -> if ePosicaoMatrizValida novaPos mapa
                                                                then if not (blocoTargeted == PedraDLC)
                                                                    then if eTerrenoDestrutivel blocoTargeted
                                                                        then EstadoDLC {mapaEstadoDLC = if blocoTargeted == TerraDLC then atualizaPosicaoMatriz novaPos ArDLC mapa else atualizaPosicaoMatriz novaPos AguaDLC mapa,objetosEstadoDLC = objetos,minhocasEstadoDLC = minhocasFinais, armaSelecionada = Just arma, minhocaSelecionada = minhocaSelecionada e, danosEstado = danosEstado e}
                                                                        else EstadoDLC {mapaEstadoDLC = mapa ,objetosEstadoDLC = objetos,minhocasEstadoDLC = minhocasFinais, armaSelecionada = Just arma, minhocaSelecionada = minhocaSelecionada e, danosEstado = danosEstado e}

                                                                    else EstadoDLC{mapaEstadoDLC=mapa,objetosEstadoDLC=objetos,minhocasEstadoDLC=atualizaIndiceLista n (minhoca{escavadoraMinhocaDLC = escavadoraMinhocaDLC minhoca-1, ultimaDirecaoHorizontal = novaDirecaoHorizontal}) minhocas, armaSelecionada = Just arma, minhocaSelecionada = minhocaSelecionada e, danosEstado = danosEstado e}
                                                                else e{minhocasEstadoDLC = minhocasFinais}

                                                        BazucaDLC -> if ePosicaoMatrizValida novaPos mapa

                                                                then EstadoDLC{mapaEstadoDLC=mapaEstadoDLC e,objetosEstadoDLC = objetosEstadoDLC e ++ [DisparoDLC{posicaoDisparoDLC=novaPos, direcaoDisparoDLC=direcao, tipoDisparoDLC=BazucaDLC, tempoDisparoDLC=Nothing, donoDisparoDLC=n}], minhocasEstadoDLC = minhocasFinais, armaSelecionada = Just arma, minhocaSelecionada = minhocaSelecionada e, danosEstado = danosEstado e}
                                                                else e{minhocasEstadoDLC = minhocasFinais}

                                                        MinaDLC -> if ePosicaoMatrizValida novaPos mapa
                                                                then (if (ePosicaoMapaLivre novaPos mapa && not (existeBarril novaPos objetos)) && ePosicaoEstadoLivre novaPos e then EstadoDLC{mapaEstadoDLC=mapaEstadoDLC e,objetosEstadoDLC = objetosEstadoDLC e ++ [DisparoDLC{posicaoDisparoDLC=novaPos, direcaoDisparoDLC=direcao, tipoDisparoDLC=MinaDLC, tempoDisparoDLC=Nothing, donoDisparoDLC=n}], minhocasEstadoDLC = minhocasFinais, armaSelecionada = Just arma, minhocaSelecionada = minhocaSelecionada e, danosEstado = danosEstado e} else EstadoDLC{mapaEstadoDLC=mapaEstadoDLC e,objetosEstadoDLC = objetosEstadoDLC e ++ [DisparoDLC{posicaoDisparoDLC=pos, direcaoDisparoDLC=direcao, tipoDisparoDLC=MinaDLC, tempoDisparoDLC=Nothing, donoDisparoDLC=n}], minhocasEstadoDLC = minhocasFinais, armaSelecionada = Just arma, minhocaSelecionada = minhocaSelecionada e, danosEstado = danosEstado e})


                                                                else e{minhocasEstadoDLC = minhocasFinais}

                                                        DinamiteDLC -> if ePosicaoMatrizValida novaPos mapa
                                                                then (if (ePosicaoMapaLivre novaPos mapa && not (existeBarril novaPos objetos)) && ePosicaoEstadoLivre novaPos e then EstadoDLC{mapaEstadoDLC=mapaEstadoDLC e,objetosEstadoDLC = objetosEstadoDLC e ++ [DisparoDLC{posicaoDisparoDLC=novaPos, direcaoDisparoDLC=direcao, tipoDisparoDLC=DinamiteDLC, tempoDisparoDLC=Just 4, donoDisparoDLC=n}], minhocasEstadoDLC = minhocasFinais, armaSelecionada = Just arma, minhocaSelecionada = minhocaSelecionada e, danosEstado = danosEstado e} else EstadoDLC{mapaEstadoDLC=mapaEstadoDLC e,objetosEstadoDLC = objetosEstadoDLC e ++ [DisparoDLC{posicaoDisparoDLC=pos, direcaoDisparoDLC=direcao, tipoDisparoDLC=DinamiteDLC, tempoDisparoDLC=Just 4, donoDisparoDLC=n}], minhocasEstadoDLC = minhocasFinais, armaSelecionada = Just arma, minhocaSelecionada = minhocaSelecionada e, danosEstado = danosEstado e})

                                                                else e{minhocasEstadoDLC = minhocasFinais}

                                                        FlameTrower -> if ePosicaoMatrizValida novaPos mapa

                                                                then EstadoDLC{mapaEstadoDLC=mapaEstadoDLC e,objetosEstadoDLC = objetosEstadoDLC e ++ [DisparoDLC{posicaoDisparoDLC=novaPos, direcaoDisparoDLC=direcao, tipoDisparoDLC=FlameTrower, tempoDisparoDLC=Nothing, donoDisparoDLC=n}], minhocasEstadoDLC = minhocasFinais, armaSelecionada = Just arma, minhocaSelecionada = minhocaSelecionada e, danosEstado = danosEstado e}
                                                                else e{minhocasEstadoDLC = minhocasFinais}

                                                    else e




                                        where


                                            minhoca = case encontraIndiceLista n minhocas of Just m -> m
                                            pos = case posicaoMinhocaDLC minhoca of Just a -> a
                                            

                                            minhocas = minhocasEstadoDLC e
                                            mapa = mapaEstadoDLC e
                                            objetos = objetosEstadoDLC e

                                            novaPos = movePosicao direcao pos

                                            novaDirecaoHorizontal = case getXWayDLC direcao of
                                                Just d -> d
                                                Nothing -> ultimaDirecaoHorizontal minhoca
                                            

                                            minhocaFinal =
                                                let
                                                    bloco =
                                                        if ePosicaoMatrizValida novaPos mapa
                                                            then encontraPosicaoMatriz novaPos mapa
                                                            else Nothing

                                                    atualizaMunicao m = case arma of
                                                        JetpackDLC    -> m { jetpackMinhocaDLC    = jetpackMinhocaDLC m - 1 }
                                                        EscavadoraDLC -> m { escavadoraMinhocaDLC = escavadoraMinhocaDLC m - 1 }
                                                        BazucaDLC     -> m { bazucaMinhocaDLC     = bazucaMinhocaDLC m - 1 }
                                                        MinaDLC       -> m { minaMinhocaDLC       = minaMinhocaDLC m - 1 }
                                                        DinamiteDLC   -> m { dinamiteMinhocaDLC   = dinamiteMinhocaDLC m - 1 }
                                                        FlameTrower   -> m { flameMinhocaDLC      = flameMinhocaDLC m - 1}

                                                    (novaPosicao, novaVida) =
                                                        if not (ePosicaoMatrizValida novaPos mapa) then
                                                            case arma of
                                                                JetpackDLC    -> (Nothing, MortaDLC)  -- jetpack fora do mapa -> morre
                                                                EscavadoraDLC -> (posicaoMinhocaDLC minhoca, vidaMinhocaDLC minhoca)  -- escavadora n mexe
                                                                _          -> (posicaoMinhocaDLC minhoca, vidaMinhocaDLC minhoca)  -- outras arma fica parada
                                                        else

                                                            case bloco of
                                                            Just AguaDLC ->
                                                                case arma of
                                                                    JetpackDLC    -> if ePosicaoEstadoLivre novaPos e then (Just novaPos, MortaDLC) else (Just pos, vidaMinhocaDLC minhoca)
                                                                    EscavadoraDLC -> if ePosicaoEstadoLivre novaPos e then (Just novaPos, MortaDLC) else (Just pos, vidaMinhocaDLC minhoca)
                                                                    _          -> (posicaoMinhocaDLC minhoca, vidaMinhocaDLC minhoca)
                                                            Just TerraDLC ->
                                                                case arma of
                                                                    JetpackDLC    -> (Just pos, vidaMinhocaDLC minhoca)
                                                                    EscavadoraDLC -> (Just novaPos, vidaMinhocaDLC minhoca)
                                                                    _          -> (posicaoMinhocaDLC minhoca, vidaMinhocaDLC minhoca)
                                                            Just PedraDLC ->
                                                                case arma of
                                                                    JetpackDLC    -> (Just pos, vidaMinhocaDLC minhoca)
                                                                    EscavadoraDLC -> (Just novaPos, vidaMinhocaDLC minhoca)
                                                                    _          -> (posicaoMinhocaDLC minhoca, vidaMinhocaDLC minhoca)
                                                            _ ->
                                                                case arma of
                                                                    JetpackDLC    ->  if ePosicaoEstadoLivre novaPos e then (Just novaPos, vidaMinhocaDLC minhoca) else (Just pos, vidaMinhocaDLC minhoca)
                                                                    EscavadoraDLC -> if estaNoSolo pos mapa minhocas objetos && ePosicaoEstadoLivre novaPos e && (novaPos `elem` posicoes8Axis pos || existeBarril novaPos objetos) 
                                                                        then (Just novaPos, vidaMinhocaDLC minhoca) 
                                                                        else (Just pos, vidaMinhocaDLC minhoca)
                                                                    _          -> (posicaoMinhocaDLC minhoca, vidaMinhocaDLC minhoca)

                                                in atualizaMunicao minhoca
                                                    { posicaoMinhocaDLC = novaPosicao
                                                    , vidaMinhocaDLC    = novaVida
                                                    , ultimaDirecaoHorizontal = novaDirecaoHorizontal
                                                    }



                                            temMunicao :: MinhocaDLC -> TipoArmaDLC -> Bool
                                            temMunicao m armaMinh = case armaMinh of
                                                JetpackDLC -> jetpackMinhocaDLC m > 0
                                                EscavadoraDLC -> escavadoraMinhocaDLC m > 0
                                                BazucaDLC -> bazucaMinhocaDLC m > 0
                                                MinaDLC -> minaMinhocaDLC m > 0
                                                DinamiteDLC -> dinamiteMinhocaDLC m > 0
                                                FlameTrower -> flameMinhocaDLC m > 0
                                                
                                            indiceValido :: Int -> EstadoDLC -> Bool
                                            indiceValido nMinhoca estado = nMinhoca <= length (minhocasEstadoDLC estado)

                                            blocoTargeted = case encontraPosicaoMatriz novaPos mapa of Just a -> a

                                            minhocasFinais = atualizaIndiceLista n minhocaFinal minhocas




-- MOVIMENTO
efetuaJogada n (Move direcao) e = if vidaMinhocaDLC minhoca /= MortaDLC && posicaoMinhocaDLC minhoca /= Nothing && (estaNoSolo pos mapa minhocas objetos || existeMinhoca (movePosicao Sul pos) minhocas)
                                                        then estadoFinal
                                                        else e



                        where
                            -- * minhoca estudada
                            minhoca = case encontraIndiceLista n minhocas of Just m -> m
                            pos = case posicaoMinhocaDLC minhoca of Just a -> a
                            _ = case vidaMinhocaDLC minhoca of VivaDLC a -> a

                            minhocas = minhocasEstadoDLC e
                            mapa = mapaEstadoDLC e
                            objetos = objetosEstadoDLC e

                            novaPos = movePosicao direcao pos

                            novaDirecaoHorizontal = case getXWayDLC direcao of
                                Just d -> d
                                Nothing -> ultimaDirecaoHorizontal minhoca
                            
                            -- * Estado final

                            minhocaFinal = -- * Nova pos n é opaca
                                if ePosicaoMatrizValida novaPos mapa
                                    then if ePosicaoEstadoLivre novaPos e
                                        then minhoca { posicaoMinhocaDLC = Just novaPos, ultimaDirecaoHorizontal = novaDirecaoHorizontal }
                                        else minhoca
                                else minhoca { posicaoMinhocaDLC = Nothing, vidaMinhocaDLC = MortaDLC, ultimaDirecaoHorizontal = novaDirecaoHorizontal }


                            minhocasFinais = atualizaIndiceLista n minhocaFinal minhocas

                            estadoFinal = EstadoDLC {
                                mapaEstadoDLC = mapa,
                                objetosEstadoDLC = objetos,
                                minhocasEstadoDLC = minhocasFinais,
                                armaSelecionada = armaSelecionada e,
                                minhocaSelecionada = minhocaSelecionada e,
                                danosEstado = danosEstado e
                            }


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
estaNoSolo :: Posicao -> MapaDLC -> [MinhocaDLC] -> [ObjetoDLC] -> Bool
estaNoSolo pos mapa minhocas obj = case encontraPosicaoMatriz (movePosicao Sul pos) mapa of
    Nothing -> False
    Just blocoInferior -> case encontraPosicaoMatriz pos mapa of
        Nothing -> False
        Just blocoAtual -> (eTerrenoOpaco blocoInferior || existeMinhocaViva (movePosicao Sul pos) minhocas || existeBarril (movePosicao Sul pos) obj) 
                           && not (eTerrenoOpaco blocoAtual)

