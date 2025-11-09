{-|
Module      : Tarefa1
Description : Validação de estados.

Módulo para a realização da Tarefa 1 de LI1\/LP1 em 2025\/26.
-}
module Tarefa1 where

import Labs2025
import Tarefa0_2025

{-
mapaValido =
        [[Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Pedra,Agua,Agua]
        ,[Terra,Terra,Terra,Terra,Pedra,Agua]
        ]
minhocasValida =
        [Minhoca {posicaoMinhoca = Just (1,1), vidaMinhoca = Viva 100, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}]
objetosValido =[]

-}

{-| Função principal da Tarefa 1. Recebe um estado e retorna se este é válido ou não.

== __Exemplo de Utilização:__

Um exemplo de estado válido seria:

@
estadoValido = Estado
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

Ao validar o estado anterior, a função retorna True

>>> validaEstado estadoValido
True


-}


validaEstado :: Estado -> Bool
validaEstado e = eMapaValido mapa && eObjetosValido e objetos && eMinhocasValidas e minhocas
    where
        mapa = mapaEstado e
        objetos = objetosEstado e
        minhocas = minhocasEstado e

-- * Funções Auxiliares

{- |A função 'eMapaValido' valida se o mapa é estruturalmente coerente.  
Um mapa é válido se todas as suas linhas têm o mesmo comprimento  
e se contém pelo menos uma linha.

@
eMapaValido [] = False    
eMapaValido m = (eMatrizValida m) 
@

== __Exemplos de Utilização:__
>>> eMapaValido [[Ar,Ar,Ar],[Ar,Ar,Ar],[Ar,Ar,Ar]]
True
>>> eMapaInvalido [[Ar,Ar,Terra],[Ar,Ar],[Ar,Ar,Terra]]
False

-}

eMapaValido :: Mapa -> Bool 
eMapaValido [] = False    
eMapaValido m = (eMatrizValida m)  


{- | A função 'eMapaValido' valida se os objetos de um estado são consistentes.  
Verifica disparos, posições válidas e coerência de donos.


== __Exemplos:__
>>> eobjetoValido Estado {mapaEstado = mapaValido, objetosEstado = [objetoValido], minhocasEstado = [minhocaValida]} [objetoValido]
True

-}

eObjetosValido :: Estado -> [Objeto] -> Bool
eObjetosValido _ [] = True
eObjetosValido e (h:t) =
    if ehDisparo h == True
        then
            if not (existeBarril (posicaoObjeto h) t)
                && objetoValido h
                && disparoValido mapa h
                && donoValido e h
            then eObjetosValido e t
            else False
        else
            if ePosicaoMapaLivre (posicaoObjeto h) mapa
                && ePosicaoEstadoLivre (posicaoObjeto h) e{objetosEstado = t}
            then eObjetosValido e t
            else False
                        
    where
        mapa = mapaEstado e

        objetoValido obj = case obj of
            Disparo{tipoDisparo = Escavadora} -> False
            Disparo{tipoDisparo = Jetpack}  -> False
            Disparo{tipoDisparo = Bazuca} -> disparoValido mapa obj
            Disparo{tipoDisparo = Mina} -> disparoValido mapa obj
            Disparo{tipoDisparo = Dinamite} -> disparoValido mapa obj
            

{-| Verifica se o 'Disparo' é valido

Funcionalidade:

* Verifica se o tempo do disparo é valido
* Verifica se a posicao do disparo é valida (so para bazuca)
* Verifica se o dono do disparo é valido
* Verifica se o disparo nao colide com outro 'Objeto' ou 'Minhoca'(so para mina e dinamite)

== __Exemplos:__

-}

disparoValido :: Mapa -> Objeto -> Bool
disparoValido m d@Disparo{tipoDisparo = Bazuca} = if (tempoDisparo d == Nothing && disparoBazucaValido d m) then True else False
disparoValido m d@Disparo{tipoDisparo = Mina} = if (tempoDisparo d <= Just 2 && tempoDisparo d >= Just 0 && ePosicaoMapaLivre (posicaoObjeto d) m) || (tempoDisparo d == Nothing && ePosicaoMapaLivre (posicaoObjeto d) m) then True else False
disparoValido m d@Disparo{tipoDisparo = Dinamite} = if (tempoDisparo d <= Just 4 && tempoDisparo d >= Just 0 && ePosicaoMapaLivre (posicaoObjeto d) m) then True else False

{-| Verifica se o disparo de bazuca é valido

Funcionalidade:
* Verifica se o bloco anterior ao disparo é livre (não opaco)
* Verifica se o disparo não colide com outro 'Objeto' ou 'Minhoca'

== __Exemplos:__

>>> disparoBazucaValido Disparo{posicaoDisparo=(3,4), direcaoDisparo=Norte, tipoDisparo=Bazuca, tempoDisparo=Nothing, donoDisparo=0} mapaValido
False
>>> disparoBazucaValido Disparo{posicaoDisparo=(1,4), direcaoDisparo=Norte, tipoDisparo=Bazuca, tempoDisparo=Nothing, donoDisparo=0} mapaValido
True


-}

disparoBazucaValido :: Objeto -> Mapa -> Bool -- e livre se n tiver opaco
disparoBazucaValido obj mapa = if (ePosicaoMapaLivre prevPos mapa) then True else False
                    where
                        pos = posicaoObjeto obj
                        dir = direcaoDisparo obj
                        prevPos = movePosicao (direcaoOposta dir) pos

-- * Verificação dos Donos

-- tem de ser um indice valido na lista das minhocas (ex 3 minhocas indices [0,1,2], logo i tem de estar no intervcalo 0 2)
-- O mesmo dono não pode ter simultaneamente mais do que um disparo de cada tipo.
-- Logo

{-| Verifica se o dono do disparo é valido

Funcionalidade:

* Verifica se o indice do dono é valido na lista de minhocas
* Verifica se o dono não tem mais do que um disparo do mesmo tipo
* Verifica se o dono existe na lista de minhocas do estado

== __Exemplos:__

@
estado1 = Estado { mapaEstado = [[Ar]], 
                   objetosEstado = [Disparo {posicaoDisparo = (1,1), 
                   direcaoDisparo = Norte, tipoDisparo = Mina, 
                   tempoDisparo = Just 2, donoDisparo = 0}], 
                   minhocasEstado = [Minhoca {posicaoMinhoca = Just (1,1), 
                   vidaMinhoca = Viva 100, 
                   jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}] }   

@
>>>donoValido estado1 (head (objetosEstado estado1))
True

-}

donoValido :: Estado -> Objeto -> Bool
donoValido e obj = if indiceValido && verificaLista (listaDonos listaDisparos) then True else False
            where
                indiceDono = donoDisparo obj
                minhocas = minhocasEstado e
                objetos = objetosEstado e
                listaDisparos = disparosEstado objetos
                -- Primeira verificação (é um indice valido na lista de minhocas)
                indiceValido = if indiceDono <= (length minhocas - 1) && indiceDono >= 0 then True else False


                -- Recebe os objetos do estado e devolve uma lista com (tipo de arma, dono)
                -- Exemplo [(Dinamite, 0,(Mina,0)] -> Valida
                -- Exemplo [(Dinamite, 0),(Dinamite,0)] -> Invalido

                
                verificaLista :: [(TipoArma, Int)] -> Bool
                verificaLista [] = True
                verificaLista (h:t) = if elem h t then False else True


                -- Devolve os objetos disparos
                disparosEstado :: [Objeto] -> [Objeto]
                disparosEstado [] = []
                disparosEstado (h:t) = if ehDisparo h then h : disparosEstado t else disparosEstado t

{-| Função auxiliar para 'donoValido'. Devolve uma lista com o tipo de arma e o dono('NumMinhoca') de cada 'Disparo'

Funcionalidade:

* Cria uma lista de tuplos ('TipoArma', 'NumMinhoca') a partir de uma lista de objetos
* Cada tuplo representa o tipo de arma e o dono do disparo correspondente

== __Exemplos:__
>>> listaDonos [Disparo{posicaoDisparo=(1,4), direcaoDisparo=Norte, tipoDisparo=Mina, tempoDisparo=Just 2, donoDisparo=0}, Disparo{posicaoDisparo=(2,3), direcaoDisparo=Sul, tipoDisparo=Dinamite, tempoDisparo=Just 3, donoDisparo=1}]
[(Mina,0),(Dinamite,1)]

-}
listaDonos :: [Objeto] -> [(TipoArma, Int)]
listaDonos [] = []
listaDonos (h:t) = (tipo,indice) : listaDonos t
                where
                    indice = donoDisparo h
                    tipo = tipoDisparo h
               



{-| Valida se as minhocas são validas

Funcionalidade:
* Verifica se a 'Posicao' da minhoca é valida
* Verifica se a morte da minhoca é valida(morteOk)
* Verifica se a vida da minhoca é valida(vidaOk)
* Verifica se as armas da minhoca são validas(armasValidas)


== __Exemplos:__
>>> eMinhocasValidas Estado {mapaEstado = mapaValido, objetosEstado = [objetoValido], minhocasEstado = [minhocaValida]} [minhocaValida]
True
-}

eMinhocasValidas :: Estado -> [Minhoca] -> Bool
eMinhocasValidas _ [] = True
eMinhocasValidas e (h:t) = posicaoValida && morteOk && vidaOk && eMinhocasValidas e t && armasValidas
  where
    posicaoValida = case posicaoMinhoca h of
        Nothing   -> True  
        Just p    -> ePosicaoEstadoLivre p e{minhocasEstado = t}

    morteOk = validaMorte h (mapaEstado e)
    vidaOk  = verificaVida h

    armasValidas = if jetpackMinhoca h >= 0 && escavadoraMinhoca h >= 0 && bazucaMinhoca h >= 0 && minaMinhoca h >= 0 && dinamiteMinhoca h >= 0 then True else False



-- *Auxiliares Minhoca


-- Verifica se a minhoca esta dentro de água, e caso esteja, verifica se a mesma esta morta
-- Retorna True se a minhoca estiver valida (pos valida e nao morta) e False caso esteja em agua e nao esteja morta

{-| Verifica se a morte da minhoca é valida

Funcionalidade:

* Verifica se a 'Posicao' da minhoca é 'Nothing' e se a vida é 'Morta'
* Verifica a 'Posicao' da minhoca e se o terreno nessa posição é 'Agua' e se a vida é 'Morta'

== __Exemplos:__
>>> validaMorte Minhoca{posicaoMinhoca=Just (5,8), vidaMinhoca=Morta, jetpackMinhoca=100, escavadoraMinhoca=200, bazucaMinhoca=150, minaMinhoca=3, dinamiteMinhoca=1} mapaValido
True

-}
validaMorte :: Minhoca -> Mapa -> Bool
validaMorte minh [] = False
validaMorte minh m = case posicaoMinhoca minh of
    Nothing ->
        vidaMinhoca minh == Morta

    Just pos -> case terrenoAtual of
                                    Agua -> vidaMinhoca minh == Morta
                                    outro    -> True
                where
                    terrenoAtual = case (encontraPosicaoMatriz pos m) of
                                        Just a -> a 
                                        Nothing -> Ar 
-- Verifica se a vida da minhoca é valida
{-| Verifica se a vida da minhoca é valida


== __Exemplos:__
>>> verificaVida Minhoca{posicaoMinhoca=Just (3,0), vidaMinhoca=Morta, jetpackMinhoca=100, escavadoraMinhoca=200, bazucaMinhoca=150, minaMinhoca=3, dinamiteMinhoca=1}
True
>>> verificaVida Minhoca{posicaoMinhoca=Just (3,0), vidaMinhoca=Viva 150, jetpackMinhoca=100, escavadoraMinhoca=200, bazucaMinhoca=150, minaMinhoca=3, dinamiteMinhoca=1}
False

-}
verificaVida :: Minhoca -> Bool
verificaVida m = case vida of
                    Morta -> True
                    Viva a -> (a >= 0 && a <= 100)
    where
        vida = vidaMinhoca m





