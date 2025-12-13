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
	| sel == 0  = return $ BotSimulation novoEstado 0 0 (0, Move Sul)  -- Iniciar jogo com jogada default
	| sel == 1  = return $ FreeRoam flatWorld 0 0 (Move Sul) -- Iniciar jogo
	| sel == 2  = return $ Help -- Tela de ajuda
	| sel == 3  = return $ Quit                     -- Sair do jogo
	| otherwise = return $ Menu sel


-- * ESC LOGIC


reageEventos (EventKey (SpecialKey KeyEsc) Down _ _) Help = return $ Menu 0 -- Voltar do Help para o menu
reageEventos (EventKey (SpecialKey KeyEnter) Down _ _) Help = return $ Menu 0 -- Voltar do jogo para o menu
reageEventos (EventKey (SpecialKey KeyEsc) Down _ _) (BotSimulation _ _ _ _) = return $ Menu 0 -- Ir do menu para quit
reageEventos (EventKey (SpecialKey KeyEsc) Down _ _) (Menu 0) = return $ Quit -- Confirmar sair do jogo
reageEventos (EventKey (SpecialKey KeyEsc) Down _ _) Quit = exitSuccess -- sai do jogo


 
-- * FREE ROAM MODE INPUTS


reageEventos (EventKey (SpecialKey KeyF1) Down _ _) (FreeRoam _ _ _ _) = exitSuccess


-- ? Movimento
reageEventos (EventKey (SpecialKey KeyEsc) Down _ _) (FreeRoam _ _ _ _) = return $ Menu 0


reageEventos (EventKey (SpecialKey KeyLeft) Down _ _) (FreeRoam est acc tick _) = 
    return $ FreeRoam (efetuaJogada 0 (Move Oeste) est) acc tick (Move Oeste)

reageEventos (EventKey (SpecialKey KeyRight) Down _ _) (FreeRoam est acc tick _) = 
    return $ FreeRoam (efetuaJogada 0 (Move Este) est) acc tick (Move Este)

reageEventos (EventKey (SpecialKey KeyUp) Down _ _) (FreeRoam est acc tick _) = 
    return $ FreeRoam (efetuaJogada 0 (Move Norte) est) acc tick (Move Norte)

reageEventos (EventKey (SpecialKey KeyDown) Down _ _) (FreeRoam est acc tick _) = 
    return $ FreeRoam (efetuaJogada 0 (Move Sul) est) acc tick (Move Sul)

-- ! Temporario

reageEventos (EventKey (Char 'q') Down _ _) (FreeRoam est acc tick _) = 
    return $ FreeRoam (efetuaJogada 0 (Move Noroeste) est) acc tick (Move Noroeste)

reageEventos (EventKey (Char 'e') Down _ _) (FreeRoam est acc tick _) = 
    return $ FreeRoam (efetuaJogada 0 (Move Nordeste) est) acc tick (Move Nordeste)

reageEventos (EventKey (Char 'z') Down _ _) (FreeRoam est acc tick _) = 
    return $ FreeRoam (efetuaJogada 0 (Move Sudoeste) est) acc tick (Move Sudoeste)

reageEventos (EventKey (Char 'c') Down _ _) (FreeRoam est acc tick _) = 
    return $ FreeRoam (efetuaJogada 0 (Move Sudeste) est) acc tick (Move Sudeste)

-- * Mudar minhoca
reageEventos (EventKey (Char '2') Down _ _) (FreeRoam est acc tick _) = 
    return $ FreeRoam (efetuaJogada 0 (Move Sudeste) est) acc tick (Move Sudeste)




-- Qualquer outro evento não altera o estado
reageEventos _ s = return $ s