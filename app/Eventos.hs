{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-|
Module      : Eventos
Description : Manipulação de eventos e interação com o utilizador.

Módulo responsável por gerir todos os eventos de entrada do utilizador (teclado),
atualizando o estado do jogo conforme as ações realizadas. Inclui navegação em menus,
criação de mapas, controlo de minhocas e seleção de armas.
-}
module Eventos where

import Graphics.Gloss.Interface.Pure.Game
import Worms
import Labs2025
import Tarefa2
import System.Exit
import Desenhar(cellSize, janelaLargura, janelaAltura)
import Tarefa0_2025 (posicaoObjeto)
import DataDLC
import EfetuaJogada
import Auxiliar (getMinhocasValidasDLC, eMinhocaVivaDLC)
import System.Directory (doesFileExist)
import Text.Read (readMaybe)
import Data.List (elemIndex)



-- * Constantes Default

{-| 'MinhocaDLC' com valores padrão para inicialização.

Valores iniciais:

* Posição: (0,0)
* Vida: 100
* Todas as armas: 100 munições
* Equipa: Blue
* Direção horizontal: Oeste
* Contador de queimadura: 0

-}
minhocaDefault :: MinhocaDLC
minhocaDefault = MinhocaDLC
    { posicaoMinhocaDLC = Just (0,0)

    , vidaMinhocaDLC = VivaDLC 100

    , jetpackMinhocaDLC = 100

    , escavadoraMinhocaDLC = 100

    , bazucaMinhocaDLC = 100

    , minaMinhocaDLC = 100

    , dinamiteMinhocaDLC = 100

    , flameMinhocaDLC = 100

    , burningCounter = 0

    , equipaMinhoca = Just Blue

    , ultimaDirecaoHorizontal = Oeste
    }

{-| 'ObjetoDLC' de tipo 'DisparoDLC' com valores padrão.

Valores iniciais:

* Posição: (0,0)
* Direção: Oeste
* Tipo: BazucaDLC
* Tempo: Nothing
* Dono: 0

-}
disparoDefault :: ObjetoDLC
disparoDefault = DisparoDLC
    {
        posicaoDisparoDLC = (0,0)

        , direcaoDisparoDLC = Oeste

        , tipoDisparoDLC = BazucaDLC

        , tempoDisparoDLC = Nothing

        , donoDisparoDLC = 0
    }



-- * Função Principal

{-| Função principal que reage aos eventos do utilizador e atualiza o estado do jogo.

Funcionalidade:

* Processa eventos de teclado ('EventKey')
* Atualiza estados de menus ('Menu', 'LevelSelector', 'Help')
* Gere jogadas no modo PvP ('PVP')
* Controla o editor de mapas ('MapCreatorTool')
* Gere navegação e seleção de opções
* Retorna o novo estado do jogo após processar o evento

== __Navegação em Menus:__

No 'Menu' principal:

* Setas direcionais: navegação entre opções
* Enter: seleção da opção atual
* ESC: sair do menu (ou retornar)

== __Modo PvP:__

* '1': trocar de minhoca
* '2': trocar de arma
* Setas WASD/QEZC: movimento e disparo
* ESC: voltar ao menu

== __Editor de Mapas:__

* Setas: navegação e seleção
* Enter: confirmar seleção
* 'e': exportar estado para ficheiro
* ESC: voltar ao menu

== __Exemplos de Utilização:__

>>> reageEventos (EventKey (SpecialKey KeyEnter) Down _ _) (Menu 0)
BotSimulation novoEstado 0 0 (0, Move Sul)

>>> reageEventos (EventKey (SpecialKey KeyEsc) Down _ _) (PVP _ _ _ _)
Menu 0

-}
reageEventos :: Event -> Worms -> IO Worms

reageEventos (EventKey (SpecialKey KeyUp) Down _ _) (Menu sel)
    | sel == 2 = return $ Menu 0  -- MAP Creator -> Bot Simulation
    | sel == 3 = return $ Menu 1  -- Help -> PvP
    | sel == 4 = return $ Menu 2  -- Quit -> MAP Creator (ou Help)
    | otherwise = return $ Menu sel

reageEventos (EventKey (SpecialKey KeyDown) Down _ _) (Menu sel)
    | sel == 0 = return $ Menu 2  -- Bot Simulation -> MAP Creator
    | sel == 1 = return $ Menu 3  -- PvP -> Help
    | sel == 2 || sel == 3 = return $ Menu 4  -- MAP Creator/Help -> Quit
    | otherwise = return $ Menu sel

reageEventos (EventKey (SpecialKey KeyLeft) Down _ _) (Menu sel)
    | sel == 1 = return $ Menu 0  -- PvP -> Bot Simulation
    | sel == 3 = return $ Menu 2  -- Help -> MAP Creator
    | otherwise = return $ Menu sel

reageEventos (EventKey (SpecialKey KeyRight) Down _ _) (Menu sel)
    | sel == 0 = return $ Menu 1  -- Bot Simulation -> PvP
    | sel == 2 = return $ Menu 3  -- MAP Creator -> Help
    | otherwise = return $ Menu sel

-- Seleção de opção no menu
reageEventos (EventKey (SpecialKey KeyEnter) Down _ _) (Menu sel)
        | sel == 0  = return $ BotSimulation novoEstado 0 0 (0, Labs2025.Move Sul)
        | sel == 1  = return $ LevelSelector 0 [level1, level2, level3]
        | sel == 2  = return $ MapCreatorTool baseEstado 0 0 0 0 False Nothing minhocaDefault disparoDefault
        | sel == 3  = return $ Help 0
        | sel == 4  = return $ Quit 1
        | otherwise = return $ Menu sel


-- * Tecla ESC - Lógica de Saída


reageEventos (EventKey (SpecialKey KeyEsc) Down _ _) (Help _) = return $ Menu 0
reageEventos (EventKey (SpecialKey KeyEnter) Down _ _) (Help _) = return $ Menu 0

reageEventos (EventKey (SpecialKey KeyLeft) Down _ _) (Help p) =
    return $ Help (max 0 (p - 1))

reageEventos (EventKey (SpecialKey KeyRight) Down _ _) (Help p) =
    return $ Help (min 5 (p + 1))

reageEventos (EventKey (SpecialKey KeyEsc) Down _ _) (MapCreatorTool {}) = return $ Menu 0
reageEventos (EventKey (SpecialKey KeyEsc) Down _ _) (BotSimulation {}) = return $ Menu 0
reageEventos (EventKey (SpecialKey KeyEsc) Down _ _) (Menu 0) = return $ Quit 1



reageEventos (EventKey (SpecialKey KeyEsc) Down _ _) (Quit _) = exitSuccess
reageEventos (EventKey (SpecialKey KeyLeft) Down _ _) (Quit sel) = return $ Quit (max 0 (sel - 1))
reageEventos (EventKey (SpecialKey KeyRight) Down _ _) (Quit sel) = return $ Quit (min 1 (sel + 1))
reageEventos (EventKey (SpecialKey KeyEnter) Down _ _) (Quit sel)
    | sel == 0 = exitSuccess
    | otherwise = return $ Menu 0


-- * Seletor de Níveis

reageEventos (EventKey (Char 'i') Down _ _) (LevelSelector i ei) = do
    existe <- doesFileExist "estado.txt"
    if not existe
        then return (LevelSelector i ei)
        else do
            conteudo <- readFile "estado.txt"
            case readMaybe conteudo :: Maybe EstadoDLC of
                Nothing -> return (LevelSelector i ei)
                Just estado -> return (LevelSelector i (ei ++ [estado]))

reageEventos (EventKey (SpecialKey KeyDown) Down _ _) (LevelSelector i ei) = -- * Down
    return $ LevelSelector (min (i + 1) (length ei - 1)) ei

reageEventos (EventKey (SpecialKey KeyUp) Down _ _) (LevelSelector i ei) = -- * Up
    return $ LevelSelector (max (i - 1) 0) ei

reageEventos (EventKey (SpecialKey KeyEnter) Down _ _) (LevelSelector i ei)
    | i >= 0 && i < length ei = return $ PVP (ei !! i) 0 0 (DataDLC.Move Sul)
    | otherwise = return $ LevelSelector i ei

reageEventos (EventKey (SpecialKey KeyEsc) Down _ _) (LevelSelector _ _) = return $ Menu 0

-- * Modo PVP - Entradas

reageEventos (EventKey (SpecialKey KeyF1) Down _ _) (PVP _ _ _ _) = exitSuccess


reageEventos (EventKey (SpecialKey KeyEsc) Down _ _) (PVP _ _ _ _) = return $ Menu 0


-- * Mudar de minhoca
reageEventos (EventKey (Char '1') Down _ _) (PVP est acc tick _) =

    let minhocaAtualIndex = minhocaSelecionada est
        proximoIndiceValido = encontraProximoIndiceValido minhocaAtualIndex (minhocasEstadoDLC est)

        nEstado = EstadoDLC {
            mapaEstadoDLC = mapaEstadoDLC est
            , minhocasEstadoDLC = minhocasEstadoDLC est
            , objetosEstadoDLC = objetosEstadoDLC est
            , armaSelecionada = Nothing
            , minhocaSelecionada = proximoIndiceValido
            , danosEstado = danosEstado est
        }

    in
        return $ PVP nEstado acc tick (DataDLC.Move Sul)




-- * Mudar arma da minhoca
reageEventos (EventKey (Char '2') Down _ _) (PVP est acc tick _) =

    let novaArma = case armaSelecionada est of
            Just JetpackDLC -> Just EscavadoraDLC
            Just EscavadoraDLC -> Just BazucaDLC
            Just BazucaDLC -> Just MinaDLC
            Just MinaDLC -> Just DinamiteDLC
            Just DinamiteDLC -> Just FlameTrower
            Just FlameTrower -> Just JetpackDLC
            Nothing -> Just JetpackDLC

        nEstado = EstadoDLC {
            mapaEstadoDLC = mapaEstadoDLC est
            , minhocasEstadoDLC = minhocasEstadoDLC est
            , objetosEstadoDLC = objetosEstadoDLC est
            , armaSelecionada = novaArma
            , minhocaSelecionada = minhocaSelecionada est
            , danosEstado = danosEstado est
        }

    in

        return $ PVP nEstado acc tick (DataDLC.Move Sul)

-- * Resto das teclas no modo PvP
reageEventos (EventKey key Down _ _) (PVP est _ _ _) =
    let (novoEst, _) = handleAction key est
        estadoFinal = verificaVitoria novoEst
    in return estadoFinal

-- * Ecrã de Game Over

reageEventos (EventKey (SpecialKey KeyEsc) Down _ _) (GameOver _) =
    return $ Menu 0




-- * Editor de Mapas (MapCreatorTool)



-- b -> bloco selecionado (lista vertical)
-- a -> modo (Blocos/Objetos/Personagens)
-- l -> Segundo Slider
-- t -> Terceiro slider


reageEventos (EventKey (SpecialKey KeyRight) Down _ _) (MapCreatorTool e 1 3 l t True c w d) =
    return $ case t of
        0 -> let newt = if (t + 1) < length axis8 then (t+1) else length axis8
             in MapCreatorTool e 1 3 l t True c w (d{direcaoDisparoDLC = axis8 !! newt})
        _ -> MapCreatorTool e 1 3 l t True c w d

reageEventos (EventKey (SpecialKey KeyLeft) Down _ _) (MapCreatorTool e 1 3 l t True c w d) =
    return $ case t of
        0 -> let newt = if (t - 1) > 0 then (t-1) else 0
             in MapCreatorTool e 1 3 l t True c w (d{direcaoDisparoDLC = axis8 !! newt})
        _ -> MapCreatorTool e 1 3 l t True c w d







reageEventos (EventKey (SpecialKey KeyRight) Down _ _) (MapCreatorTool e 3 1 l t True i w d) =
    let
        direcaoAtual = direcaoDisparoDLC d
        indiceAtual = case elemIndex direcaoAtual axis8 of
            Just idx -> idx
            Nothing -> 0
        proximoIndice = (indiceAtual + 1) `mod` 8
        novaDirecao = axis8 !! proximoIndice

        tempoAtual = tempoDisparoDLC d
        novoTempo = case tempoAtual of
                Nothing -> Just 0
                Just n -> Just (n + 1)


    in case t of
        0 -> return $ MapCreatorTool e 3 1 l t True i w (d{direcaoDisparoDLC = novaDirecao})
        1 -> return $ MapCreatorTool e 3 1 l t True i w (d{tempoDisparoDLC = novoTempo})
        2 -> return $ MapCreatorTool e 3 1 l t True i w (d{donoDisparoDLC = if (donoDisparoDLC d + 1) >= length (minhocasEstadoDLC e) then donoDisparoDLC d else donoDisparoDLC d + 1})
        _ -> return $ MapCreatorTool e 3 1 l t True i w d


reageEventos (EventKey (SpecialKey KeyLeft) Down _ _) (MapCreatorTool e 3 1 l t True i w d) =
    let
        direcaoAtual = direcaoDisparoDLC d
        indiceAtual = case elemIndex direcaoAtual axis8 of
            Just idx -> idx
            Nothing -> 0
        proximoIndice = (indiceAtual - 1) `mod` 8
        novaDirecao = axis8 !! proximoIndice


        tempoAtual = tempoDisparoDLC d
        novoTempo = case tempoAtual of
            Nothing -> Nothing
            Just n -> if n > 0 then Just (n - 1) else Nothing
    in case t of
        0 -> return $ MapCreatorTool e 3 1 l t True i w (d{direcaoDisparoDLC = novaDirecao})
        1 -> return $ MapCreatorTool e 3 1 l t True i w (d{tempoDisparoDLC = novoTempo})
        2 -> return $ MapCreatorTool e 3 1 l t True i w (d{donoDisparoDLC = max (donoDisparoDLC d - 1) 0})
        _ -> return $ MapCreatorTool e 3 1 l t True i w d



reageEventos (EventKey (Char char) Down _ _) (MapCreatorTool e b 2 l t True _ w d) =
    return $
        if char `elem` ['0'..'9']
        then
            let num = charParaInt char
            in case t of
                    0 -> MapCreatorTool e b 2 l t True (Just num) (w{jetpackMinhocaDLC = read (show (jetpackMinhocaDLC w) ++ [char]) :: Int}) d
                    1 -> MapCreatorTool e b 2 l t True (Just num) (w{escavadoraMinhocaDLC = read (show (escavadoraMinhocaDLC w) ++ [char]) :: Int}) d
                    2 -> MapCreatorTool e b 2 l t True (Just num) (w{bazucaMinhocaDLC = read (show (bazucaMinhocaDLC w) ++ [char]) :: Int}) d
                    3 -> MapCreatorTool e b 2 l t True (Just num) (w{minaMinhocaDLC = read (show (minaMinhocaDLC w) ++ [char]) :: Int}) d
                    4 -> MapCreatorTool e b 2 l t True (Just num) (w{dinamiteMinhocaDLC = read (show (dinamiteMinhocaDLC w) ++ [char]) :: Int}) d
                    5 -> MapCreatorTool e b 2 l t True (Just num) (w{flameMinhocaDLC = read (show (flameMinhocaDLC w) ++ [char]) :: Int}) d
                    6 -> MapCreatorTool e b 2 l t True (Just num) (w{burningCounter = read (show (burningCounter w) ++ [char]) :: Int}) d
                    _ -> MapCreatorTool e b 2 l t True (Just num) w d
        else MapCreatorTool e b 2 l t True Nothing w d

-- * Tanto delete ou backspace servem 

reageEventos (EventKey (SpecialKey KeyBackspace) Down _ _) (MapCreatorTool e b 2 l t True c w d) =
    return $ case t of
        0 -> MapCreatorTool e b 2 l t True c (w{jetpackMinhocaDLC = max 0 (jetpackMinhocaDLC w `div` 10)}) d
        1 -> MapCreatorTool e b 2 l t True c (w{escavadoraMinhocaDLC = max 0 (escavadoraMinhocaDLC w `div` 10)}) d
        2 -> MapCreatorTool e b 2 l t True c (w{bazucaMinhocaDLC = max 0 (bazucaMinhocaDLC w `div` 10)}) d
        3 -> MapCreatorTool e b 2 l t True c (w{minaMinhocaDLC = max 0 (minaMinhocaDLC w `div` 10)}) d
        4 -> MapCreatorTool e b 2 l t True c (w{dinamiteMinhocaDLC = max 0 (dinamiteMinhocaDLC w `div` 10)}) d
        5 -> MapCreatorTool e b 2 l t True c (w{flameMinhocaDLC = max 0 (flameMinhocaDLC w `div` 10)}) d
        6 -> MapCreatorTool e b 2 l t True c (w{burningCounter = max 0 (burningCounter w `div` 10)}) d
        _ -> MapCreatorTool e b 2 l t True c w d

reageEventos (EventKey (SpecialKey KeyDelete) Down _ _) (MapCreatorTool e b 2 l t True c w d) =
    return $ case t of
        0 -> MapCreatorTool e b 2 l t True c (w{jetpackMinhocaDLC = max 0 (jetpackMinhocaDLC w `div` 10)}) d
        1 -> MapCreatorTool e b 2 l t True c (w{escavadoraMinhocaDLC = max 0 (escavadoraMinhocaDLC w `div` 10)}) d
        2 -> MapCreatorTool e b 2 l t True c (w{bazucaMinhocaDLC = max 0 (bazucaMinhocaDLC w `div` 10)}) d
        3 -> MapCreatorTool e b 2 l t True c (w{minaMinhocaDLC = max 0 (minaMinhocaDLC w `div` 10)}) d
        4 -> MapCreatorTool e b 2 l t True c (w{dinamiteMinhocaDLC = max 0 (dinamiteMinhocaDLC w `div` 10)}) d
        5 -> MapCreatorTool e b 2 l t True c (w{flameMinhocaDLC = max 0 (flameMinhocaDLC w `div` 10)}) d
        6 -> MapCreatorTool e b 2 l t True c (w{burningCounter = max 0 (burningCounter w `div` 10)}) d
        _ -> MapCreatorTool e b 2 l t True c w d

reageEventos (EventKey (SpecialKey KeyRight) Down _ _) (MapCreatorTool e b 2 l t True c w d) =
    return $ case t of
        7 -> MapCreatorTool e b 2 l t True c (w{equipaMinhoca = Just Red}) d
        _ -> MapCreatorTool e b 2 l t True c w d

reageEventos (EventKey (SpecialKey KeyLeft) Down _ _) (MapCreatorTool e b 2 l t True c w d) =
    return $ case t of
        7 -> MapCreatorTool e b 2 l t True c (w{equipaMinhoca = Just Blue}) d
        _ -> MapCreatorTool e b 2 l t True c w d

-- *

-- AUmentar Linhas -> 
reageEventos (EventKey (Char 'k') Down _ _) (MapCreatorTool e b a l t ed c w d) =
    let
        mapa = mapaEstadoDLC e
        colunas = if null mapa then 0 else length (head mapa)
        novaLinha = replicate colunas ArDLC
        novoMapa = mapa ++ [novaLinha]
    in return $ MapCreatorTool e{mapaEstadoDLC = novoMapa} b a l t ed c w d

reageEventos (EventKey (Char 'l') Down _ _) (MapCreatorTool e b a l t ed c w d) =
    let
        mapa = mapaEstadoDLC e
        novoMapa = map (++ [ArDLC]) mapa
    in return $ MapCreatorTool e{mapaEstadoDLC = novoMapa} b a  l t ed c w d


reageEventos (EventKey (Char 'n') Down _ _) (MapCreatorTool e b a l t ed c w d) =
    let
        mapa = mapaEstadoDLC e
        novoMapa = init mapa
    in return $ MapCreatorTool e{mapaEstadoDLC = novoMapa} b a l t ed c w d

reageEventos (EventKey (Char 'm') Down _ _) (MapCreatorTool e b a l t ed c w d) =
    let
        mapa = mapaEstadoDLC e
        novoMapa = map init  mapa
    in return $ MapCreatorTool e{mapaEstadoDLC = novoMapa} b a l t ed c w d



reageEventos (EventKey (Char '1') Down _ _) (MapCreatorTool e _ a l t ed c w d) =
    let novoa = if a > 1 then 0 else a + 1
    in return $ MapCreatorTool e 0 novoa l t ed c w d




reageEventos (EventKey (MouseButton LeftButton) Down _ mousePos) (MapCreatorTool e blocoSelecionado modo l t ed c w d) =
    let
        mapa = mapaEstadoDLC e
        (mx, my) = mousePos
        sidebarWidth = 300
        linha = length mapa
        cols = if null mapa then 0 else length (head mapa)
        largura = fromIntegral cols * cellSize
        altura = fromIntegral linha * cellSize
        usableWidth = janelaLargura - sidebarWidth
        usableHeight = janelaAltura
        sx = if largura > 0 then usableWidth / largura else 1
        sy = if altura > 0 then usableHeight / altura else 1
        scaleFactor = min (min sx sy) 2.0
        offsetX = sidebarWidth / 2
        worldX = (mx - offsetX) / scaleFactor
        worldY = my / scaleFactor
        colIdx = floor ((worldX + (largura / 2)) / cellSize)
        linhaIdx = floor (((altura / 2) - worldY) / cellSize)
        posicao = (linhaIdx, colIdx)

        -- Verifica se a posição é válida
        posValida = linhaIdx >= 0 && linhaIdx < linha && colIdx >= 0 && colIdx < cols

        nEstado = if posValida
            then case modo of
                0 -> e { mapaEstadoDLC = atualizaMapa mapa linhaIdx colIdx (getBlocoFromIndex blocoSelecionado) }
                1 -> adicionaObjetoDLC e blocoSelecionado posicao l d
                2 -> adicionaMinhocaDLC e posicao w
                _ -> e
            else e
    in return $ MapCreatorTool nEstado blocoSelecionado modo l t ed c w d


reageEventos (EventKey (MouseButton RightButton) Down _ mousePos) (MapCreatorTool e blocoSelecionado modo l t ed c w d) =
    let
        mapa = mapaEstadoDLC e
        (mx, my) = mousePos
        sidebarWidth = 300
        linha = length mapa
        cols = if null mapa then 0 else length (head mapa)
        largura = fromIntegral cols * cellSize
        altura = fromIntegral linha * cellSize
        usableWidth = janelaLargura - sidebarWidth
        usableHeight = janelaAltura
        sx = if largura > 0 then usableWidth / largura else 1
        sy = if altura > 0 then usableHeight / altura else 1
        scaleFactor = min (min sx sy) 2.0
        offsetX = sidebarWidth / 2
        worldX = (mx - offsetX) / scaleFactor
        worldY = my / scaleFactor
        colIdx = floor ((worldX + (largura / 2)) / cellSize)
        linhaIdx = floor (((altura / 2) - worldY) / cellSize)
        posicao = (linhaIdx, colIdx)

        -- Verifica se a posição é válida
        posValida = linhaIdx >= 0 && linhaIdx < linha && colIdx >= 0 && colIdx < cols

        nEstado = if posValida
            then case modo of
                0 -> e { mapaEstadoDLC = atualizaMapa mapa linhaIdx colIdx ArDLC }
                1 -> removeObjetoDLC e posicao
                2 -> removeMinhocaDLC e posicao
                _ -> e
            else e
    in return $ MapCreatorTool nEstado blocoSelecionado modo l t ed c w d



reageEventos (EventKey (SpecialKey KeyUp) Down _ _) (MapCreatorTool e b a _ _ False c w d) = -- * UP
    let
        x = case a of
            0 -> 5
            1 -> 3
            2 -> 0
            _ -> 0 -- !
        novob = if b >= x || b <= 0 then 0 else b - 1

    in return $ MapCreatorTool e novob a 0 0 False c w d

reageEventos (EventKey (SpecialKey KeyDown) Down _ _) (MapCreatorTool e b a _ _ False c w d) = -- * DOWN
    let
        x = case a of
            0 -> 5
            1 -> 3
            2 -> 0
            _ -> 0 -- !
        novob = if b >= x then 0 else b + 1

    in return $ MapCreatorTool e novob a 0 0 False c w d


reageEventos (EventKey (Char '2') Down _ _) (MapCreatorTool e b a _ _ False c w d) = -- * DOWN V2
    let
        x = case a of
            0 -> 5
            1 -> 3
            2 -> 0
            _ -> 0 -- !
        novob = if b >= x then 0 else b + 1

    in return $ MapCreatorTool e novob a 0 0 False c w d

reageEventos (EventKey (SpecialKey KeyLeft) Down _ _) (MapCreatorTool e b a l t edit c w d) =  -- * <
    let

        novol
            | (a == 1 && b == 2) = (l - 1) `mod` 5 -- a = 1 é modo objetos, b = 2 sao os ammo packs
            | (a == 1 && b == 3) = if edit then l else (l - 1) `mod` 4 -- b = 3 sao os disparos
            | (a == 2) = 0
            | otherwise = l

        novot
            | a == 2 && l == 0 = (t - 1) `mod` 8
            | otherwise = 0

    in if novol <= 0 then return $ MapCreatorTool e b a 0 novot edit c w d else return $ MapCreatorTool e b a novol novot edit c w d

reageEventos (EventKey (SpecialKey KeyRight) Down _ _) (MapCreatorTool e b a l t edit c w d) =  -- * >
    let
        disparos = [BazucaDLC, MinaDLC, DinamiteDLC, FlameTrower]
        novol
            | (a == 1 && b == 2) = (l + 1) `mod` 5 -- a = 1 é modo objetos, b = 2 sao os ammo packs
            | (a == 1 && b == 3) = if edit then l else (l + 1) `mod` 4 -- b = 3 sao os disparos
            | (a == 2) = 0
            | otherwise = l

        novot
            | a == 2 && l == 0 = (t + 1) `mod` 8
            | otherwise = 0

    in if novol <= 0 then return $ MapCreatorTool e b a 0 novot edit c w d else return $ MapCreatorTool e b a novol novot edit c w (d{tipoDisparoDLC = disparos !! l})




reageEventos (EventKey (SpecialKey KeyDown) Down _ _) (MapCreatorTool e 3 1 l t edit c w d) =
    let

        novot = if edit then (t + 1) `mod` 3 else t


    in return $ MapCreatorTool e 3 1 l novot edit c w d

reageEventos (EventKey (SpecialKey KeyUp) Down _ _) (MapCreatorTool e 3 1 l t edit c w d) =
    let

        novot = if edit then (t - 1) `mod` 3 else 0

    in return $ MapCreatorTool e 3 1 l novot edit c w d








reageEventos (EventKey (SpecialKey KeyEnter) Down _ _) (MapCreatorTool e b 2 l t ed c w d) =
    return $ MapCreatorTool e b 2 l t (not ed) c w d


reageEventos (EventKey (SpecialKey KeyEnter) Down _ _) (MapCreatorTool e b 1 l t ed c w d) =
    return $ MapCreatorTool e b 1 l t (not ed) c w d

reageEventos (EventKey (Char 'e') Down _ _) m@(MapCreatorTool e _ _ _ _ _ _ _ _) = do
    writeFile "estado.txt" (show e)
    return m










-- Qualquer outro evento não altera o estado
reageEventos _ s = return s





















-- * Funções Auxiliares

{-| Adiciona um 'Objeto' ao 'Estado' numa determinada 'Posicao'.

Funcionalidade:

* Remove qualquer objeto existente na mesma posição
* Adiciona o novo objeto conforme o índice fornecido

== __Exemplos:__

>>> adicionaObjeto estadoBase 0 (1,1)
Estado com Barril em (1,1)

-}
adicionaObjeto :: Estado -> Int -> Posicao -> Estado
adicionaObjeto e idx pos =
    let novoObjeto = case idx of
            0 -> Barril pos False
            _ -> Barril pos False
        objetosAtuais = objetosEstado e
        objetosFiltrados = filter (\obj -> Tarefa0_2025.posicaoObjeto obj /= pos) objetosAtuais
    in e { objetosEstado = objetosFiltrados ++ [novoObjeto] }

{-| Adiciona uma 'Minhoca' ao 'Estado' numa determinada 'Posicao'.

Funcionalidade:

* Remove qualquer minhoca existente na mesma posição
* Cria uma nova minhoca com valores padrão (100 de vida e munições)
* Adiciona a nova minhoca à lista de minhocas do estado

== __Exemplos:__

>>> adicionaMinhoca estadoBase (2,3)
Estado com nova Minhoca em (2,3)

-}
adicionaMinhoca :: Estado -> Posicao -> Estado
adicionaMinhoca e pos =
    let minhocasAtuais = minhocasEstado e
        minhocasFiltradas = filter (\m -> posicaoMinhoca m /= Just pos) minhocasAtuais
        novaMinhoca = Minhoca {posicaoMinhoca = Just pos, vidaMinhoca = Viva 100, jetpackMinhoca = 100, escavadoraMinhoca = 100, bazucaMinhoca=100, minaMinhoca = 100, dinamiteMinhoca=100}
    in e { minhocasEstado = minhocasFiltradas ++ [novaMinhoca] }




{-| Converte uma tecla ('Key') numa 'Direcao'.

Mapeamento de teclas:

* Setas direcionais: 4 direções principais
* WASD: 4 direções principais
* QEZC: 4 direções diagonais (Noroeste, Nordeste, Sudoeste, Sudeste)

== __Exemplos:__

>>> keyToDirection (SpecialKey KeyUp)
Norte

>>> keyToDirection (Char 'q')
Noroeste

-}
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

{-| Converte um índice ('Int') num tipo de 'TerrenoDLC'.

Mapeamento de índices:

* 0: TerraDLC
* 1: AguaDLC
* 2: PedraDLC
* 3: ArDLC
* 4: Lava
* 5: Gelo

== __Exemplos:__

>>> getBlocoFromIndex 0
TerraDLC

>>> getBlocoFromIndex 4
Lava

-}
getBlocoFromIndex :: Int -> TerrenoDLC
getBlocoFromIndex 0 = TerraDLC
getBlocoFromIndex 1 = AguaDLC
getBlocoFromIndex 2 = PedraDLC
getBlocoFromIndex 3 = ArDLC
getBlocoFromIndex 4 = Lava
getBlocoFromIndex 5 = Gelo
getBlocoFromIndex _ = ArDLC




{-| Atualiza um 'MapaDLC' substituindo o 'TerrenoDLC' numa posição específica.

Funcionalidade:

* Divide o mapa na linha especificada
* Atualiza a coluna dentro dessa linha
* Reconstrói o mapa com a alteração

== __Exemplos:__

>>> atualizaMapa mapaBase 2 3 PedraDLC
MapaDLC com Pedra na posição (2,3)

-}
atualizaMapa :: MapaDLC -> Int -> Int -> TerrenoDLC -> MapaDLC
atualizaMapa mapa linha col novoBloco =
    let (antes, linhaAtual:depois) = splitAt linha mapa
        novaLinha = atualizaLinha linhaAtual col novoBloco
    in antes ++ [novaLinha] ++ depois

{-| Atualiza uma linha do mapa substituindo um 'TerrenoDLC' numa coluna específica.

Funcionalidade:

* Divide a linha na coluna especificada
* Substitui o terreno nessa coluna
* Reconstrói a linha com a alteração

== __Exemplos:__

>>> atualizaLinha [ArDLC, ArDLC, ArDLC] 1 TerraDLC
[ArDLC, TerraDLC, ArDLC]

-}
atualizaLinha :: [TerrenoDLC] -> Int -> TerrenoDLC -> [TerrenoDLC]
atualizaLinha linha col novoBloco =
    let (antes, _:depois) = splitAt col linha
    in antes ++ [novoBloco] ++ depois


{-| Verifica se a 'Minhoca' pode mover-se (está no solo).

Funcionalidade:

* Verifica se o índice da minhoca é válido
* Obtém a posição atual da minhoca
* Usa 'estaNoSolo' para verificar se está apoiada em terreno

== __Exemplos:__

>>> podeMover estadoComMinhocaNoSolo 0
True

>>> podeMover estadoComMinhocaNoAr 0
False

-}
podeMover :: Estado -> NumMinhoca -> Bool
podeMover est i =
    case encontraIndiceLista i (minhocasEstado est) of
        Nothing -> False
        Just minhoca ->
            case posicaoMinhoca minhoca of
                Nothing -> False
                Just pos -> Tarefa2.estaNoSolo pos (mapaEstado est)



{-| Processa uma ação do utilizador (tecla pressionada) e atualiza o 'EstadoDLC'.

Funcionalidade:

* Se há arma selecionada: executa disparo na direção da tecla
* Se não há arma selecionada: executa movimento na direção da tecla
* Retorna o novo estado e a jogada realizada

== __Exemplos:__

>>> handleAction (SpecialKey KeyUp) estadoComArmaSelecionada
(novoEstado, Dispara JetpackDLC Norte)

>>> handleAction (Char 'w') estadoSemArmaSelecionada
(novoEstado, Move Norte)

-}
handleAction :: Key -> EstadoDLC -> (EstadoDLC, JogadaDLC)
handleAction key est =
    let
        maybeArma = armaSelecionada est
        i = minhocaSelecionada est

        dir = keyToDirection key
    in case maybeArma of

        Just arma ->
            let novoEst = EfetuaJogada.efetuaJogada i (DataDLC.Dispara arma dir) est
            in (novoEst, DataDLC.Dispara arma dir)

        Nothing ->
            let novoEst = EfetuaJogada.efetuaJogada i (DataDLC.Move dir) est
                 in (novoEst, DataDLC.Move dir)
           


-- * Funções Auxiliares DLC

{-| Adiciona um 'ObjetoDLC' ao 'EstadoDLC' numa determinada 'Posicao'.

Funcionalidade:

* Remove qualquer objeto existente na mesma posição
* Cria novo objeto conforme índices fornecidos:
    * 0: BarrilDLC
    * 1: HealthPack
    * 2: AmmoPack (tipo depende de secSel)
    * 3: DisparoDLC (tipo depende de secSel)
* Adiciona o novo objeto à lista de objetos

== __Exemplos:__

>>> adicionaObjetoDLC estadoBase 0 (1,1) 0 disparoDefault
EstadoDLC com BarrilDLC em (1,1)

>>> adicionaObjetoDLC estadoBase 2 (2,2) 3 disparoDefault
EstadoDLC com AmmoPack de MinaDLC em (2,2)

-}
adicionaObjetoDLC :: EstadoDLC -> Int -> Posicao -> Int -> ObjetoDLC -> EstadoDLC
adicionaObjetoDLC e idx pos secSel obj =
    let novoObjeto = case idx of
            0 -> BarrilDLC pos False
            1 -> HealthPack pos 50
            2 ->let ammoPacks = [JetpackDLC, EscavadoraDLC, BazucaDLC, MinaDLC, DinamiteDLC]
                    am = ammoPacks !! (secSel `mod` 5)
                in AmmoPack pos 50 am
            3 ->
                let disparos = [BazucaDLC, MinaDLC, DinamiteDLC, FlameTrower]
                    tipo = disparos !! (secSel `mod` 4)
                    time = tempoDisparoDLC obj
                    dir = direcaoDisparoDLC obj
                    dono = donoDisparoDLC obj

                in DisparoDLC { posicaoDisparoDLC = pos, direcaoDisparoDLC = dir, tipoDisparoDLC = tipo, tempoDisparoDLC = time, donoDisparoDLC = dono } -- ! isto tem valores defaults que tem de ser mudados
            _ -> BarrilDLC pos False
        objetosAtuais = objetosEstadoDLC e

        objetosFiltrados = filter (\objeto -> DataDLC.posicaoObjeto objeto /= pos) objetosAtuais
    in e { objetosEstadoDLC = objetosFiltrados ++ [novoObjeto] }

{-| Adiciona uma 'MinhocaDLC' ao 'EstadoDLC' numa determinada 'Posicao'.

Funcionalidade:

* Remove qualquer minhoca existente na mesma posição
* Cria nova minhoca com os atributos da minhoca fornecida
* Define a última direção horizontal como Oeste
* Adiciona a nova minhoca à lista de minhocas

== __Exemplos:__

>>> adicionaMinhocaDLC estadoBase (3,3) minhocaDefault
EstadoDLC com nova MinhocaDLC em (3,3)

-}
adicionaMinhocaDLC :: EstadoDLC -> Posicao -> MinhocaDLC -> EstadoDLC
adicionaMinhocaDLC e pos minhoca =
    let minhocasAtuais = minhocasEstadoDLC e

        minhocasFiltradas = filter (\m -> posicaoMinhocaDLC m /= Just pos) minhocasAtuais

        novaMinhoca = MinhocaDLC {posicaoMinhocaDLC = Just pos,
        vidaMinhocaDLC = vidaMinhocaDLC minhoca,
        jetpackMinhocaDLC = jetpackMinhocaDLC minhoca,
        escavadoraMinhocaDLC = escavadoraMinhocaDLC minhoca,
        bazucaMinhocaDLC=bazucaMinhocaDLC minhoca,
        minaMinhocaDLC = minaMinhocaDLC minhoca,
        dinamiteMinhocaDLC=dinamiteMinhocaDLC minhoca,
        flameMinhocaDLC = flameMinhocaDLC minhoca,
        burningCounter = burningCounter minhoca,
        equipaMinhoca = equipaMinhoca minhoca,
        ultimaDirecaoHorizontal = Oeste}




    in e { minhocasEstadoDLC = minhocasFiltradas ++ [novaMinhoca] }  -- ! isto tem valores defaults que tem de ser mudados

{-| Remove um 'ObjetoDLC' do 'EstadoDLC' numa determinada 'Posicao'.

Funcionalidade:

* Filtra a lista de objetos removendo o objeto na posição especificada
* Retorna o estado atualizado sem o objeto

== __Exemplos:__

>>> removeObjetoDLC estadoComObjeto (1,1)
EstadoDLC sem objeto em (1,1)

-}
removeObjetoDLC :: EstadoDLC -> Posicao -> EstadoDLC
removeObjetoDLC e pos =
    let objetosAtuais = objetosEstadoDLC e
        objetosFiltrados = filter (\obj -> DataDLC.posicaoObjeto obj /= pos) objetosAtuais
    in e { objetosEstadoDLC = objetosFiltrados }

{-| Remove uma 'MinhocaDLC' do 'EstadoDLC' numa determinada 'Posicao'.

Funcionalidade:

* Filtra a lista de minhocas removendo a minhoca na posição especificada
* Retorna o estado atualizado sem a minhoca

== __Exemplos:__

>>> removeMinhocaDLC estadoComMinhoca (2,2)
EstadoDLC sem minhoca em (2,2)

-}
removeMinhocaDLC :: EstadoDLC -> Posicao -> EstadoDLC
removeMinhocaDLC e pos =
    let minhocasAtuais = minhocasEstadoDLC e
        minhocasFiltradas = filter (\m -> posicaoMinhocaDLC m /= Just pos) minhocasAtuais
    in e { minhocasEstadoDLC = minhocasFiltradas }


{-| Encontra o próximo índice de uma 'MinhocaDLC' viva após o índice atual.

Funcionalidade:

* Filtra apenas minhocas vivas
* Procura o próximo índice válido maior que o atual
* Se não encontrar, retorna ao primeiro índice válido
* Se não houver minhocas vivas, retorna 0

== __Exemplos:__

>>> encontraProximoIndiceValido 0 [minhocaViva1, minhocaMorta, minhocaViva2]
2

>>> encontraProximoIndiceValido 2 [minhocaViva1, minhocaMorta, minhocaViva2]
0

-}
encontraProximoIndiceValido :: Int -> [MinhocaDLC] -> Int
encontraProximoIndiceValido atual minhocas =
    let indicesValidos = [i | (i, m) <- zip [0..] minhocas, eMinhocaVivaDLC m]
    in if null indicesValidos
       then 0
       else
           let
               proximosIndices = filter (> atual) indicesValidos
           in if null proximosIndices
              then head indicesValidos
              else head proximosIndices

{-| Verifica as condições de vitória no modo PvP.

Funcionalidade:

* Conta minhocas vivas de cada equipa (Red e Blue)
* Retorna 'GameOver' com a equipa vencedora se:
    * Todas as minhocas Red morreram (Blue vence)
    * Todas as minhocas Blue morreram (Red vence)
    * Ambas as equipas morreram (empate, Red por padrão)
* Retorna 'PVP' com o estado atual se o jogo continua

== __Exemplos:__

>>> verificaVitoria estadoComApenasBlueViva
GameOver Blue

>>> verificaVitoria estadoComAmbosVivos
PVP estadoAtual 0 0 (Move Sul)

-}
verificaVitoria :: EstadoDLC -> Worms
verificaVitoria est =
    let minhocasVivas = getMinhocasValidasDLC (minhocasEstadoDLC est)
        minhocasRed = filter (\m -> equipaMinhoca m == Just Red) minhocasVivas
        minhocasBlue = filter (\m -> equipaMinhoca m == Just Blue) minhocasVivas
    in case (null minhocasRed, null minhocasBlue) of
        (True, False) -> GameOver Blue  -- Blue venceu
        (False, True) -> GameOver Red   -- Red venceu
        (True, True)  -> GameOver Red   -- Empate (ou escolhe uma equipa)
        _             -> PVP est 0 0 (DataDLC.Move Sul)  -- Jogo continua



{-| Converte um 'Char' para 'Int'.

Funcionalidade:

* Lê o caractere como string unitária
* Converte para Int usando read

== __Exemplos:__

>>> charParaInt '5'
5

>>> charParaInt '0'
0

-}
charParaInt :: Char -> Int
charParaInt c = read [c] :: Int