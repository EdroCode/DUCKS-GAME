{-|
Module      : Tarefa1
Description : Validação de estados.

Módulo para a realização da Tarefa 1 de LI1\/LP1 em 2025\/26.
-}
module Tarefa1 where

import Labs2025
import Tarefa0_2025



-- * Aula

getMunicoesMinhoca :: Minhoca -> Int
getMunicoesMinhoca m = bazucaMinhoca m

incrementMunBazuca :: Minhoca -> Int -> Minhoca
incrementMunBazuca m i = m {bazucaMinhoca = bazucaMinhoca m + i}
-- -incrementMunBazuca m@Minhoca {bazucaMinhoca = bazucas} i - m {bazucaMinhoca = bazucas}

-- * ------------------------------------------------------------


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
eObjetosValido e (h:t) = if ePosicaoMapaLivre (posicaoObjeto h) mapa 
                        && ePosicaoEstadoLivre (posicaoObjeto h) e{objetosEstado = t} 
                        && objetoValido h
                        && disparoValido h
                        && verificarDonos donos
                        then eObjetosValido e t else False
    where
        mapa = mapaEstado e
        minhocas = minhocasEstado e
        donos = listaDonos (h:t)
     

        objetoValido obj = case obj of
            Disparo{tipoDisparo = Escavadora} -> False
            Disparo{tipoDisparo = Jetpack}  -> False
            Disparo{tipoDisparo = Bazuca} -> disparoValido obj
            Disparo{tipoDisparo = Mina} -> disparoValido obj
            Disparo{tipoDisparo = Dinamite} -> disparoValido obj
            _             -> True --todo caso exista novos casos adicionar
        
        
-- * ---------------- Auxiliares Objeto  ---------------------------

-- * Verifica se o objeto fornecido é um disparo

ehDisparo :: Objeto -> Bool
ehDisparo d@Disparo{} = True
ehDisparo _ = False

-- * Verifica se o disparo é valido

disparoValido :: Objeto -> Bool
disparoValido d@Disparo{tipoDisparo = Bazuca} = if (tempoDisparo d == Nothing) then True else False
disparoValido d@Disparo{tipoDisparo = Mina} = if (tempoDisparo d <= Just 2 && tempoDisparo d >= Just 0) then True else False
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

verificarDonos :: [Int] -> Bool
verificarDonos [] = True
verificarDonos (h:t) = if existe h t then False else verificarDonos t
                      
existe :: Eq a => a -> [a] -> Bool
existe _ [] = False
existe x (h:t)
  | x == h    = True
  | otherwise = existe x t

-- * -------------------------------------------

-- * Valida se as minhocas são validas

eMinhocasValidas :: Estado -> [Minhoca] -> Bool
eMinhocasValidas e [] = True
eMinhocasValidas e (h:t) = if ePosicaoEstadoLivre pos e{minhocasEstado = t} 
                           && validaMorte h (mapaEstado e)
                           && verificaVida h
                           then eMinhocasValidas e t else False
    where
        pos = case posicaoMinhoca h of 
            Just (y,x) -> (y,x) 
            Nothing -> (-1,-1) -- !  perguntar ao stor
        vida = case vidaMinhoca h of Viva a -> a
        municoes = [jetpackMinhoca, escavadoraMinhoca, bazucaMinhoca, minaMinhoca, dinamiteMinhoca]
        muniValidas :: [Int] -> Bool
        muniValidas [] = True
        muniValidas (h:t) = if (h >= 0) then muniValidas t else False



-- * ---------------- Auxiliares Minhoca  ---------------------------


-- * Verifica se a minhoca esta dentro de água, e caso esteja, verifica se a mesma esta morta
-- * Retorna True se a minhoca estiver valida (pos valida e nao morta) e False caso esteja em agua e nao esteja morta
validaMorte :: Minhoca -> Mapa -> Bool
validaMorte minh [] = False
validaMorte minh m = if pos == (-1, -1) || terrenoAtual == Agua -- ! mesma situacao
                        then if minhocaViva then True else False
                        else True                      
  where
    
    vida = case vidaMinhoca minh of
        Morta -> 0
        Viva a -> a
    
    pos = case posicaoMinhoca minh of Just (y,x) -> (y,x)
    terrenoAtual = case (encontraPosicaoMatriz pos m) of Just a -> a 
                                                         Nothing -> Ar
    
    minhocaViva :: Bool
    minhocaViva = if vida == 0 then False else True


-- * Verifica se a vida da minhoca é valida

verificaVida :: Minhoca -> Bool
verificaVida m = case vida of
                    Morta -> True
                    Viva a -> if (a >= 0 && a <= 100) then True else False
    where
        vida = vidaMinhoca m


-- todo -- Fazer com que caso a minhoca tenha vida 0 morra (pode vir a ser feito em tarefas futuras entao perguntar)