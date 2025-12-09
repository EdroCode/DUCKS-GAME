module Eventos where

import Graphics.Gloss.Interface.Pure.Game
import Worms
import Labs2025
import Tarefa2
import System.Exit

-- | Função principal que reage aos eventos do usuário e atualiza o estado do jogo
reageEventos :: Event -> Worms -> IO Worms


-- Navegação no menu principal
reageEventos (EventKey (SpecialKey KeyUp) Down _ _) (Menu sel)
	| sel > 0   = return $ Menu (sel - 1)
	| otherwise = return $ Menu sel
reageEventos (EventKey (SpecialKey KeyDown) Down _ _) (Menu sel)
	| sel < 3   = return $ Menu (sel + 1)
	| otherwise = return $ Menu sel

-- Seleção de opção no menu
reageEventos (EventKey (SpecialKey KeyEnter) Down _ _) (Menu sel)
	| sel == 0  = return $ BotSimulation novoEstado 0 0  -- Iniciar jogo
	| sel == 1  = return $ FreeRoam flatWorld 0 0 (Move Sul) -- Iniciar jogo
	| sel == 2  = return $ Help -- Tela de ajuda
	| sel == 3  = return $ Quit                     -- Sair do jogo
	                -- Sair do jogo
	| otherwise = return $ Menu sel


-- Voltar do Help para o menu
reageEventos (EventKey (SpecialKey KeyEsc) Down _ _) Help = return $ Menu 0
reageEventos (EventKey (SpecialKey KeyEnter) Down _ _) Help = return $ Menu 0
-- Voltar do jogo para o menu
reageEventos (EventKey (SpecialKey KeyEsc) Down _ _) (BotSimulation _ _ _) = return $ Menu 0

-- Ir do menu para quit
reageEventos (EventKey (SpecialKey KeyEsc) Down _ _) (Menu 0) = return $ Quit

-- | Confirmar sair do jogo
reageEventos (EventKey (SpecialKey KeyEsc) Down _ _) Quit = exitSuccess




-- Controlo da minhoca em FreeRoam
reageEventos (EventKey (SpecialKey KeyLeft) Down _ _) (FreeRoam est acc tick _) =
    return $ FreeRoam est acc tick (Move Oeste)

reageEventos (EventKey (SpecialKey KeyRight) Down _ _) (FreeRoam est acc tick _) =
    return $ FreeRoam est acc tick (Move Este)

reageEventos (EventKey (SpecialKey KeyUp) Down _ _) (FreeRoam est acc tick _) =
    return $ FreeRoam est acc tick (Move Norte)

reageEventos (EventKey (SpecialKey KeyDown) Down _ _) (FreeRoam est acc tick _) =
    return $ FreeRoam est acc tick (Move Sul)

-- Parar movimento quando larga as teclas
reageEventos (EventKey (SpecialKey _) Up _ _) (FreeRoam est acc tick _) =
    return $ FreeRoam est acc tick (Move Sul)



-- Qualquer outro evento não altera o estado
reageEventos _ s = return $ s







-- | Remove disparos pertencentes a minhocas que já morreram
filtraDisparos :: [Objeto] -> [Minhoca] -> [Objeto]
filtraDisparos objs ms = filter disparoValido objs
  where
	mortos = pegaIndicesMortos ms 0
	-- Retorna os índices das minhocas mortas
	pegaIndicesMortos [] _ = []
	pegaIndicesMortos (m:rest) i =
	  let r = pegaIndicesMortos rest (i+1)
	  in case vidaMinhoca m of
		   Morta -> i : r
		   _     -> r
	-- Verifica se o disparo pertence a uma minhoca viva
	disparoValido o@(Disparo {}) = notElem (donoDisparo o) mortos
	disparoValido _ = True
