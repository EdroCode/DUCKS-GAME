{-|
Module      : Tarefa1
Description : Validação de estados.

Módulo para a realização da Tarefa 1 de LI1\/LP1 em 2025\/26.
-}
module Tarefa1 where

import Labs2025
import Tarefa0_2025






-- | Função principa*! Excecao nl da Tarefa 1. Recebe um estado e retorna se este é válido ou não.
validaEstado :: Estado -> Bool
validaEstado e = eMapaValido mapa && eObjetosValido e objetos && eMinhocasValidas e minhocas
    where
        mapa = mapaEstado e
        objetos = objetosEstado e
        minhocas = minhocasEstado e

-- * Valida se o mapa é valido

eMapaValido :: Mapa -> Bool 
eMapaValido [] = False    
eMapaValido m = (eMatrizValida m)  

-- * -------------------------------------------

-- * Valida se o objeto é valido

eObjetosValido :: Estado -> [Objeto] -> Bool
eObjetosValido _ [] = True
eObjetosValido e (h:t) =
    if ehDisparo h == True
    then
        if ePosicaoMapaLivre (posicaoObjeto h) mapa
            && not (existeBarril (posicaoObjeto h) t)
            && objetoValido h
            && disparoValido h
            && verificarDonos donos
            && donosValido
        then eObjetosValido e t
        else False
    else
        if ePosicaoMapaLivre (posicaoObjeto h) mapa
            && ePosicaoEstadoLivre (posicaoObjeto h) e{objetosEstado = t}
            && objetoValido h
            && disparoValido h
            && verificarDonos donos
            && donosValido
        then eObjetosValido e t
        else False

                        
    where
        mapa = mapaEstado e
        minhocas = minhocasEstado e -- * MINHOCAS
        donos = listaDonos (h:t)

        donosValido = if length donos > length minhocas then False else True

        objetoValido obj = case obj of
            Disparo{tipoDisparo = Escavadora} -> False
            Disparo{tipoDisparo = Jetpack}  -> False
            Disparo{tipoDisparo = Bazuca} -> disparoValido obj
            Disparo{tipoDisparo = Mina} -> disparoValido obj
            Disparo{tipoDisparo = Dinamite} -> disparoValido obj
            _             -> True --todo caso exista novos casos adicionar
        
        
-- * ---------------- Auxiliares Objeto  ---------------------------



-- * Verifica se o disparo é valido

disparoValido :: Objeto -> Bool
disparoValido d@Disparo{tipoDisparo = Bazuca} = if (tempoDisparo d == Nothing) then True else False
disparoValido d@Disparo{tipoDisparo = Mina} = if (tempoDisparo d <= Just 2 && tempoDisparo d >= Just 0) || (tempoDisparo d == Nothing) then True else False
disparoValido d@Disparo{tipoDisparo = Dinamite} = if (tempoDisparo d <= Just 4 && tempoDisparo d >= Just 0) then True else False
disparoValido _ = True

-- * Retorna uma lista de todos os donos dos disparos no estado

listaDonos :: [Objeto] -> [Int]
listaDonos [] = []
listaDonos (h:t) = if ehDisparo h
                        then [x] ++ listaDonos t 
                        else listaDonos t
                    where
                        x = donoDisparo h

-- * Verifica se existe donos repetidos

verificarDonos :: Eq Int => [Int] -> Bool
verificarDonos [] = True
verificarDonos (h:t) = if elem h t then False else verificarDonos t

-- * -------------------------------------------

-- * Valida se as minhocas são validas

eMinhocasValidas :: Estado -> [Minhoca] -> Bool
eMinhocasValidas _ [] = True
eMinhocasValidas e (h:t) = posicaoValida && morteOk && vidaOk && eMinhocasValidas e t
  where
    posicaoValida = case posicaoMinhoca h of
        Nothing   -> True  
        Just p    -> ePosicaoEstadoLivre p e{minhocasEstado = t}

    morteOk = validaMorte h (mapaEstado e)
    vidaOk  = verificaVida h



-- * ---------------- Auxiliares Minhoca  ---------------------------


-- * Verifica se a minhoca esta dentro de água, e caso esteja, verifica se a mesma esta morta
-- * Retorna True se a minhoca estiver valida (pos valida e nao morta) e False caso esteja em agua e nao esteja morta

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
                                        Nothing -> Ar -- ! atencao
-- * Verifica se a vida da minhoca é valida

verificaVida :: Minhoca -> Bool
verificaVida m = case vida of
                    Morta -> True
                    Viva a -> if (a >= 0 && a <= 100) then True else False
    where
        vida = vidaMinhoca m


-- todo -> Disparos podem ser repetidos caso nao sejam do mesmo dono
-- todo bloco anterior ao disparo da bazuca
-- todo UMA MINA E UMA DINAMITE PODEM ESTAR NA POSICAO DA MINHOCA OU OUTRO OBJETO ( FEITO )