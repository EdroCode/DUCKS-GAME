module Tempo where

import Worms
import Labs2025
import Tarefa0_geral
import Tarefa0_2025
import Tarefa2
import Tarefa3
import Tarefa4


-- | Tempo em segundos.
type Segundos = Float

-- | Intervalo entre passos automáticos (segundos).

stepInterval :: Segundos
stepInterval = 1

-- | Função que avança o tempo no estado do jogo no Gloss.
reageTempo :: Segundos -> Worms -> Worms
reageTempo _ m@Menu{} = m  -- sem mudança enquanto está no menu
reageTempo _ Help = Help
reageTempo _ Quit = Quit
reageTempo dt (Playing est acc tick) = Playing estFinal acc' tick'
	where
		-- acumula segundos desde o último passo e calcula quantos
		-- passos (ticks) devem ser processados agora
		acc2 = acc + dt
		steps = floor (acc2 / stepInterval)
		acc' = acc2 - fromIntegral steps * stepInterval
		tick' = tick + steps

		-- aplica os passos necessários (cada passo = jogada tática + atualização de objetos)
		estFinal = applySteps est tick steps

		-- | Aplica n passos sequenciais ao estado. Cada passo tem um número de tick
		-- usado pela jogada tática para decidir a ação.
		applySteps :: Estado -> Int -> Int -> Estado
		applySteps st _ 0 = st
		applySteps st t n = applySteps (applyOne t st) (t + 1) (n - 1)

		applyOne :: Int -> Estado -> Estado
		-- Aplica um passo: primeiro a jogada tática (quem/jogada), depois
		-- atualiza o estado físico (objetos em movimento) através de 'avancaEstado'.
		applyOne t st = avancaEstado $ efetuaJogada jogador jogada st
			where
				(jogador, jogada) = jogadaTatica t st
	