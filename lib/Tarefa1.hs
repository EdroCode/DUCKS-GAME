{-|
Module      : Tarefa1
Description : Validação de estados.

Módulo para a realização da Tarefa 1 de LI1\/LP1 em 2025\/26.
-}
module Tarefa1 where

import Labs2025
import Tarefa0_2025




getMunicoesMinhoca :: Minhoca -> Int
getMunicoesMinhoca m = bazucaMinhoca m

incrementMunBazuca :: Minhoca -> Int -> Minhoca
incrementMunBazuca m i = m {bazucaMinhoca = bazucaMinhoca m + i}
-- -incrementMunBazuca m@Minhoca {bazucaMinhoca = bazucas} i - m {bazucaMinhoca = bazucas}

-- | Função principal da Tarefa 1. Recebe um estado e retorna se este é válido ou não.
validaEstado :: Estado -> Bool
validaEstado e = eMapaValido mapa
    where
        mapa = mapaEstado e


-- *todo   ValidacaoMapa

eMapaValido :: Mapa -> Bool -- * Mapa é valido 
eMapaValido [] = False      -- * a) Nao Vazio 
eMapaValido m = (tamanhoMapaValido m && temTerrenoValido m)  -- * b) é uma grelha (l = c)
                            -- * c) contem terrenos validos

tamanhoMapaValido :: Mapa -> Bool
tamanhoMapaValido [] = False
tamanhoMapaValido [h] = True
tamanhoMapaValido (h1:h2:t) = length h1 == length h2 && tamanhoMapaValido (h2:t)


temTerrenoValido :: Mapa -> Bool
temTerrenoValido [] = False
temTerrenoValido m = all (all terrenoValido) m

terrenoValido :: Terreno -> Bool
terrenoValido ter = True -- *! Verificar com stor porque [[CImento]] -> erro e nao False


-- *todo  ValidacaoObjeto


-- * Objeto é valido se (barril) -> posicao Valida e livre (menos disparos)
-- *                                posicao nao tem minhoca ou barril (sem ser o msm) (exceto bazuca)
-- *! Excecao nao feita - bazuca
-- * Se for um disparo -> Nao pode ser jetpack ou escavadora
-- * O tempo do disparo tem que ser coerente com o tipo de arma (bazuca = sem tempo) 
-- * (mina sem tempo, ou um inteiro entre 0 e 2) (dinamite inteiro entre 0 e 4)
-- * o dono do disparo tem de ser um indice valido

eObjetosValido :: Estado -> [Objeto] -> Bool
eObjetosValido e [] = True
eObjetosValido e (h:t) = if (ePosicaoMapaLivre posicaoObjeto mapa && temBarril) then eObjetosValido e t else False
    where
        posicaoObjeto = case h of
            Disparo { posicaoDisparo = p } -> p
            Barril  { posicaoBarril  = p } -> p

        mapa = mapaEstado e
        minhocas = minhocasEstado e
        temBarril = case h of
            Disparo { tipoDisparo = Bazuca } -> True --*! Rever depois -- Testa se a bala for bazuca entao pode ter barril ou minhoca la que nao interfere
            outro -> not (existeMinhoca posicaoObjeto minhocas) && not (existeObjeto posicaoObjeto (h:t))


ehDisparo :: Objeto -> Bool
ehDisparo Disparo{} = True
ehDisparo _         = False






