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
validaEstado e = eMapaValido mapa && eObjetosValido e objetos
    where
        mapa = mapaEstado e
        objetos = objetosEstado e

    
-- *todo   ValidacaoMapa

eMapaValido :: Mapa -> Bool -- * Mapa é valido 
eMapaValido [] = False      -- * a) Nao Vazio 
eMapaValido m = (eMatrizValida m)  -- * b) é uma grelha (l = c)
                            -- * c) contem terrenos validos


-- *todo  ValidacaoObjeto


-- * Objeto é valido se (barril) -> posicao Valida e livre (menos disparos)
-- *                                posicao nao tem minhoca ou barril (sem ser o msm) (exceto bazuca)
-- ao feita - bazuca
-- * Se for um disparo -> Nao pode ser jetpack ou escavadora
-- * O tempo do disparo tem que ser coerente com o tipo de arma (bazuca = sem tempo) 
-- * (mina sem tempo, ou um inteiro entre 0 e 2) (dinamite inteiro entre 0 e 4)
-- * o dono do disparo tem de ser um indice valido
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




-- todo -- verificar a bala

  