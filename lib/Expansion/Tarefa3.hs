{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-|
Module      : Expansion.Tarefa3
Description : Avançar tempo do jogo.

Módulo para a realização da Tarefa 3 de LI1\/LP1 em 2025\/26.
-}

module Expansion.Tarefa3 where

import Data.Either
import Expansion.Tarefa0_geral
import Expansion.Tarefa0_2025
import Expansion.Tarefa2
import Expansion.DataDLC
import GHC.Generics ((:+:)(R1))
import Tarefa3(Dano, Danos)