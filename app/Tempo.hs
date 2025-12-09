{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Tempo where

import Worms
import Labs2025
import Tarefa0_geral
import Tarefa0_2025
import Tarefa2
import Tarefa3
import Tarefa4






type Segundos = Float

-- | Intervalo entre passos automático.

intervalo :: Segundos
intervalo = 0.2

-- | Função que avança o tempo no estado do jogo no Gloss.
reageTempo :: Segundos -> Worms -> IO Worms
reageTempo _ m@Menu{} = return m  -- sem mudança enquanto está no menu
reageTempo _ Help = return Help
reageTempo _ Quit = return Quit
reageTempo dt (BotSimulation est acc tick) = return $ BotSimulation estFinal acc' tick'
        where
                acc2 = acc + dt
                steps = floor (acc2 / intervalo)
                acc' = acc2 - fromIntegral steps * intervalo
                tick' = tick + steps

                -- aplica os passos necessários (cada passo = jogada tática + atualização de objetos)
                estFinal = aplicaPassos est tick steps

                -- | Aplica n passos sequenciais ao estado. Cada passo tem um número de tick
                -- usado pela jogada tática para decidir a ação.
                aplicaPassos :: Estado -> Int -> Int -> Estado
                aplicaPassos st _ 0 = st
                aplicaPassos st t n = aplicaPassos (aplicaUm t st) (t + 1) (n - 1)

                aplicaUm :: Int -> Estado -> Estado
                -- Aplica um passo: primeiro a jogada tática (quem/jogada), depois avança o estado
                aplicaUm t st = avancaEstado $ efetuaJogada jogador jogada st
                        where
                                (jogador, jogada) = jogadaTatica t st
reageTempo dt (FreeRoam est acc tick jogadaUser) =
    return $ FreeRoam estFinal acc' tick' jogadaUser
  where
    acc2  = acc + dt
    steps = floor (acc2 / intervalo)
    acc'  = acc2 - fromIntegral steps * intervalo
    tick' = tick + steps

    estFinal = aplicaPassos est steps

    -- Aplica n passos usando a jogada do utilizador
    aplicaPassos :: Estado -> Int -> Estado
    aplicaPassos st 0 = st
    aplicaPassos st n = aplicaPassos (aplicaUm st jogadaUser) (n-1)

    -- Aplica um passo: efetua a jogada do jogador e avança o estado
    aplicaUm :: Estado -> Jogada -> Estado
    aplicaUm st jog = avancaEstado (efetuaJogada 0 jog st) -- ! indice ta 0, depois fazer para mudar
