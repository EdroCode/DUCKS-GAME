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
intervalo = 1

-- | Função que avança o tempo no estado do jogo no Gloss.
reageTempo :: Segundos -> Worms -> IO Worms
reageTempo _ m@Menu{} = return m  -- sem mudança enquanto está no menu
reageTempo _ Help = return Help
reageTempo _ Quit = return Quit
reageTempo dt (BotSimulation est acc tick ultimaJogada) = return $ BotSimulation estFinal acc' tick' novaJogada
        where
                acc2 = acc + dt
                steps = floor (acc2 / intervalo)
                acc' = acc2 - fromIntegral steps * intervalo
                tick' = tick + steps

		-- aplica os passos necessários (cada passo = jogada tática + atualização de objetos)
		(estFinal, novaJogada) = aplicaPassos est tick steps ultimaJogada

		-- | Aplica n passos sequenciais ao estado. Cada passo tem um número de tick
		-- usado pela jogada tática para decidir a ação.
		-- Retorna o estado final e a última jogada executada
		aplicaPassos :: Estado -> Int -> Int -> (NumMinhoca, Jogada) -> (Estado, (NumMinhoca, Jogada))
		aplicaPassos st _ 0 lastJogada = (st, lastJogada)
		aplicaPassos st t n lastJogada = aplicaPassos estNovo (t + 1) (n - 1) jogadaAtual
                        where
                                (estNovo, jogadaAtual) = aplicaUm t st

                aplicaUm :: Int -> Estado -> (Estado, (NumMinhoca, Jogada))
                -- Aplica um passo: primeiro a jogada tática (quem/jogada), depois avança o estado
                -- Retorna o novo estado e a jogada executada
                aplicaUm t st = (avancaEstado $ efetuaJogada jogador jogada st, (jogador, jogada))
                        where
                                (jogador, jogada) = jogadaTatica t st
                                
reageTempo dt (FreeRoam est acc tick jogadaUser) = return $ FreeRoam (avancaEstado est) acc tick jogadaUser
