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



-- | Função principal da Tarefa 1. Recebe um estado e retorna se este é válido ou não.
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
-- *! Excecao nao feita - bazuca
-- * Se for um disparo -> Nao pode ser jetpack ou escavadora
-- * O tempo do disparo tem que ser coerente com o tipo de arma (bazuca = sem tempo) 
-- * (mina sem tempo, ou um inteiro entre 0 e 2) (dinamite inteiro entre 0 e 4)
-- * o dono do disparo tem de ser um indice valido
eObjetosValido :: Estado -> [Objeto] -> Bool
eObjetosValido _ [] = True
eObjetosValido e (h:t) = if ePosicaoMapaLivre posicaoObjeto mapa && ePosicaoEstadoLivre posicaoObjeto e && (not (objetoInvalido h)) then eObjetosValido e t else False
    where
    mapa = mapaEstado e
    minhocas = minhocasEstado e

    posicaoObjeto = case h of
        Disparo { posicaoDisparo = p } -> p
        Barril  { posicaoBarril  = p } -> p
    
    objetoInvalido obj = case obj of
        Disparo{tipoDisparo = Escavadora} -> True
        Disparo{tipoDisparo = Jetpack}  -> True
        _             -> False --todo caso exista novos casos adicionar
    
    
    

ehDisparo :: Objeto -> Bool
ehDisparo Disparo{} = True
ehDisparo _ = False


-- todo 

