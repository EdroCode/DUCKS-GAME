module Eventos where

import Graphics.Gloss.Interface.Pure.Game
import Worms
import Labs2025
import Tarefa2
import System.Exit
import Tarefa4 (minhocaOnSight, getMinhocasValidas)



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
	| sel == 1  = return $ PVP flatWorld 0 0 (Move Sul) -- Iniciar jogo
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


reageEventos (EventKey (SpecialKey KeyF1) Down _ _) (PVP _ _ _ _) = exitSuccess


reageEventos (EventKey (SpecialKey KeyEsc) Down _ _) (PVP _ _ _ _) = return $ Menu 0


-- * Mudar de minhoca
reageEventos (EventKey (Char '1') Down _ _) (PVP est acc tick _) = 

    let novaMinhoca = if (minhocaSelecionada est + 1) > (length minhocasValidas - 1) then 0 else (minhocaSelecionada est + 1)

        minhocasValidas = getMinhocasValidas (minhocasEstado est)
        
        novoEstado = Estado {
            mapaEstado = mapaEstado est
            , minhocasEstado = minhocasEstado est
            , objetosEstado = objetosEstado est
            , armaSelecionada = Nothing
            , minhocaSelecionada = novaMinhoca
        }    

    in

        return $ PVP novoEstado acc tick (Move Sul)


-- * Mudar arma minhoca
reageEventos (EventKey (Char '2') Down _ _) (PVP est acc tick _) = 

    let novaArma = case armaSelecionada est of
            Just Jetpack -> Just Escavadora
            Just Escavadora -> Just Bazuca
            Just Bazuca -> Just Mina
            Just Mina -> Just Dinamite
            Just Dinamite -> Nothing
            Nothing -> Just Jetpack

        novoEstado = Estado {
            mapaEstado = mapaEstado est
            , minhocasEstado = minhocasEstado est
            , objetosEstado = objetosEstado est
            , armaSelecionada = novaArma
            , minhocaSelecionada = minhocaSelecionada est
        }    

    in

        return $ PVP novoEstado acc tick (Move Sul)


-- * RESTO DE DOWNS
reageEventos (EventKey key Down _ _) (PVP est acc tick _) = 
    let (novoEst, jogada) = handleAction key est
    in return $ PVP novoEst acc tick jogada






-- Qualquer outro evento não altera o estado
reageEventos _ s = return $ s





-- Função auxiliar para determinar direção baseada na tecla
keyToDirection :: Key -> Direcao
keyToDirection key = case key of
    SpecialKey KeyUp    -> Norte
    SpecialKey KeyDown  -> Sul
    SpecialKey KeyLeft  -> Oeste
    SpecialKey KeyRight -> Este
    Char 'w'            -> Norte
    Char 's'            -> Sul
    Char 'a'            -> Oeste
    Char 'd'            -> Este
    Char 'q'            -> Noroeste
    Char 'e'            -> Nordeste
    Char 'z'            -> Sudoeste
    Char 'c'            -> Sudeste
    _                   -> Sul


-- | Verifica se a minhoca pode se mover (está no solo)
podeMover :: Estado -> NumMinhoca -> Bool
podeMover est i = 
    case encontraIndiceLista i (minhocasEstado est) of
        Nothing -> False
        Just minhoca -> 
            case posicaoMinhoca minhoca of
                Nothing -> False
                Just pos -> estaNoSolo pos (mapaEstado est)


handleAction :: Key -> Estado -> (Estado, Jogada)
handleAction key est = 
    let
        maybeArma = armaSelecionada est
        i = minhocaSelecionada est
        dir = keyToDirection key
    in case maybeArma of
        -- Se tem arma selecionada, sempre dispara
        Just arma -> 
            let novoEst = efetuaJogada i (Dispara arma dir) est
            in (novoEst, Dispara arma dir)
        
        -- Se não tem arma, só move se estiver no solo
        Nothing -> 
            if podeMover est i
            then let novoEst = efetuaJogada i (Move dir) est
                 in (novoEst, Move dir)
            else (est, Move dir)