module Eventos where

import Graphics.Gloss.Interface.Pure.Game
import Worms
import Labs2025
import Tarefa2
import System.Exit
import Tarefa4 (minhocaOnSight, getMinhocasValidas)
import Desenhar(cellSize, janelaLargura, janelaAltura)
import Tarefa0_2025 (posicaoObjeto)
import DataDLC
import EfetuaJogada
import AvancaEstado
import Auxiliar (getMinhocasValidasDLC, eMinhocaVivaDLC)
import System.IO





-- | Função principal que reage aos eventos do usuário e atualiza o estado do jogo
reageEventos :: Event -> Worms -> IO Worms

-- Navegação no menu principal com layout 2x2

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
        | sel == 1  = return $ LevelSelector 0
    | sel == 2  = return $ MapCreatorTool flatWorld 0 0 0
        | sel == 3  = return $ Help
        | sel == 4  = return $ Quit
        | otherwise = return $ Menu sel


-- * ESC LOGIC


reageEventos (EventKey (SpecialKey KeyEsc) Down _ _) Help = return $ Menu 0
reageEventos (EventKey (SpecialKey KeyEnter) Down _ _) Help = return $ Menu 0
reageEventos (EventKey (SpecialKey KeyEsc) Down _ _) (MapCreatorTool _ _ _ _) = return $ Menu 0
reageEventos (EventKey (SpecialKey KeyEsc) Down _ _) (BotSimulation _ _ _ _) = return $ Menu 0
reageEventos (EventKey (SpecialKey KeyEsc) Down _ _) (Menu 0) = return $ Quit
reageEventos (EventKey (SpecialKey KeyEsc) Down _ _) Quit = exitSuccess


-- * LVL SELECTOR

reageEventos (EventKey (SpecialKey KeyDown) Down _ _) (LevelSelector i) =
    return $ LevelSelector (i + 1)

reageEventos (EventKey (SpecialKey KeyUp) Down _ _) (LevelSelector i) =
    return $ LevelSelector (i - 1)

reageEventos (EventKey (SpecialKey KeyEnter) Down _ _) (LevelSelector i)
        | i == 0  = return $ PVP flatWorld 0 0 (DataDLC.Move Sul)
        | i == 1  = return $ PVP flatWorld 0 0 (DataDLC.Move Sul)
        | otherwise = return $ LevelSelector i

reageEventos (EventKey (SpecialKey KeyEsc) Down _ _) (LevelSelector _) = return $ Menu 0

-- * PVP MODE INPUTS

reageEventos (EventKey (SpecialKey KeyF1) Down _ _) (PVP _ _ _ _) = exitSuccess


reageEventos (EventKey (SpecialKey KeyEsc) Down _ _) (PVP _ _ _ _) = return $ Menu 0


-- * Mudar de minhoca
reageEventos (EventKey (Char '1') Down _ _) (PVP est acc tick _) =

    let minhocasValidas = getMinhocasValidasDLC (minhocasEstadoDLC est)
        minhocaAtualIndex = minhocaSelecionada est


        proximoIndiceValido = encontraProximoIndiceValido minhocaAtualIndex (minhocasEstadoDLC est)

        novoEstado = EstadoDLC {
            mapaEstadoDLC = mapaEstadoDLC est
            , minhocasEstadoDLC = minhocasEstadoDLC est
            , objetosEstadoDLC = objetosEstadoDLC est
            , armaSelecionada = Nothing
            , minhocaSelecionada = proximoIndiceValido
        }

    in
        return $ PVP novoEstado acc tick (DataDLC.Move Sul)




-- * Mudar arma minhoca
reageEventos (EventKey (Char '2') Down _ _) (PVP est acc tick _) =

    let novaArma = case armaSelecionada est of
            Just JetpackDLC -> Just EscavadoraDLC
            Just EscavadoraDLC -> Just BazucaDLC
            Just BazucaDLC -> Just MinaDLC
            Just MinaDLC -> Just DinamiteDLC
            Just DinamiteDLC -> Nothing
            Nothing -> Just JetpackDLC

        novoEstado = EstadoDLC {
            mapaEstadoDLC = mapaEstadoDLC est
            , minhocasEstadoDLC = minhocasEstadoDLC est
            , objetosEstadoDLC = objetosEstadoDLC est
            , armaSelecionada = novaArma
            , minhocaSelecionada = minhocaSelecionada est
        }

    in

        return $ PVP novoEstado acc tick (DataDLC.Move Sul)

-- * RESTO DE DOWNS pvp
reageEventos (EventKey key Down _ _) (PVP est acc tick _) =
    let (novoEst, jogada) = handleAction key est
        estadoFinal = verificaVitoria novoEst
    in return estadoFinal

-- * GAME OVER SCREEN 

reageEventos (EventKey (SpecialKey KeyEsc) Down _ _) (GameOver _) =
    return $ Menu 0




-- * MAP CREATOR


-- AUmentar Linhas -> 
reageEventos (EventKey (Char 'k') Down _ _) (MapCreatorTool e b a l) =
    let
        mapa = mapaEstadoDLC e
        linhas = length mapa
        colunas = if null mapa then 0 else length (head mapa)
        novaLinha = replicate colunas ArDLC
        novoMapa = mapa ++ [novaLinha]
    in return $ MapCreatorTool e{mapaEstadoDLC = novoMapa} b a l

reageEventos (EventKey (Char '1') Down _ _) (MapCreatorTool e b a l) =
    let
        mapa = mapaEstadoDLC e
        novoa = if a > 1 then 0 else a + 1
    in return $ MapCreatorTool e 0 novoa 0

reageEventos (EventKey (Char 'n') Down _ _) (MapCreatorTool e b a l) =
    let
        mapa = mapaEstadoDLC e
        linhas = length mapa
        colunas = if null mapa then 0 else length (head mapa)
        novaLinha = replicate colunas ArDLC
        novoMapa = init mapa
    in return $ MapCreatorTool e{mapaEstadoDLC = novoMapa} b a l

reageEventos (EventKey (Char 'm') Down _ _) (MapCreatorTool e b a l) =
    let
        mapa = mapaEstadoDLC e
        novoMapa = map init  mapa
    in return $ MapCreatorTool e{mapaEstadoDLC = novoMapa} b a l

reageEventos (EventKey (SpecialKey KeyUp) Down _ _) (MapCreatorTool e b a _) =
    let
        x = case a of
            0 -> 4
            1 -> 7
            2 -> 0
        novob = if b >= x || b <= 0 then 0 else b - 1

    in return $ MapCreatorTool e novob a 0

reageEventos (EventKey (SpecialKey KeyDown) Down _ _) (MapCreatorTool e b a _) =
    let
        x = case a of
            0 -> 4
            1 -> 7
            2 -> 0
        novob = if b >= x then 0 else b + 1

    in return $ MapCreatorTool e novob a 0

reageEventos (EventKey (Char '1') Down _ _) (MapCreatorTool e b a l) =
    let
        mapa = mapaEstadoDLC e
        novoa = if a > 1 then 0 else a + 1
    in return $ MapCreatorTool e 0 novoa l




reageEventos (EventKey (MouseButton LeftButton) Down _ mousePos) (MapCreatorTool e blocoSelecionado modo l) =
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

        novoEstado = if posValida
            then case modo of
                0 -> e { mapaEstadoDLC = atualizaMapa mapa linhaIdx colIdx (getBlocoFromIndex blocoSelecionado) }
                1 -> adicionaObjetoDLC e blocoSelecionado posicao l
                2 -> adicionaMinhocaDLC e posicao
                _ -> e
            else e
    in return $ MapCreatorTool novoEstado blocoSelecionado modo l


reageEventos (EventKey (MouseButton RightButton) Down _ mousePos) (MapCreatorTool e blocoSelecionado modo l) =
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

        novoEstado = if posValida
            then case modo of
                0 -> e { mapaEstadoDLC = atualizaMapa mapa linhaIdx colIdx ArDLC }
                1 -> removeObjetoDLC e posicao
                2 -> removeMinhocaDLC e posicao
                _ -> e
            else e
    in return $ MapCreatorTool novoEstado blocoSelecionado modo l



reageEventos (EventKey (SpecialKey KeyLeft) Down _ _) (MapCreatorTool e b a l) =  -- * <
    let

        novol
            | (a == 1 && b == 2) = (l - 1) `mod` 5 -- a = 1 é modo objetos, b = 2 sao os ammo packs
            | (a == 1 && b == 3) = (l - 1) `mod` 4 -- b = 3 sao os disparos
            | otherwise = l

    in if novol <= 0 then return $ MapCreatorTool e b a 0 else return $ MapCreatorTool e b a novol

reageEventos (EventKey (SpecialKey KeyRight) Down _ _) (MapCreatorTool e b a l) =  -- * >
    let

        novol
            | (a == 1 && b == 2) = (l + 1) `mod` 5 -- a = 1 é modo objetos, b = 2 sao os ammo packs
            | (a == 1 && b == 3) = (l + 1) `mod` 4 -- b = 3 sao os disparos
            | otherwise = l

    in if novol <= 0 then return $ MapCreatorTool e b a 0 else return $ MapCreatorTool e b a novol

reageEventos (EventKey (SpecialKey KeyEnter) Down _ _) m@(MapCreatorTool e _ _ _) = do
    writeFile "estado.txt" (show e)
    return m










-- Qualquer outro evento não altera o estado
reageEventos _ s = return s





















-- * Funções auxiliares

adicionaObjeto :: Estado -> Int -> Posicao -> Estado
adicionaObjeto e idx pos =
    let novoObjeto = case idx of
            0 -> Barril pos False
            _ -> Barril pos False
        objetosAtuais = objetosEstado e
        -- Remove objeto existente na mesma posição (se houver)
        objetosFiltrados = filter (\obj -> Tarefa0_2025.posicaoObjeto obj /= pos) objetosAtuais
    in e { objetosEstado = objetosFiltrados ++ [novoObjeto] }

adicionaMinhoca :: Estado -> Posicao -> Estado
adicionaMinhoca e pos =
    let minhocasAtuais = minhocasEstado e
        -- Remove minhoca existente na mesma posição (se houver)
        minhocasFiltradas = filter (\m -> posicaoMinhoca m /= Just pos) minhocasAtuais
        -- Cria nova minhoca
        novaMinhoca = Minhoca {posicaoMinhoca = Just pos, vidaMinhoca = Viva 100, jetpackMinhoca = 100, bazucaMinhoca=100, minaMinhoca = 100, dinamiteMinhoca=100}
    in e { minhocasEstado = minhocasFiltradas ++ [novaMinhoca] }




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

getBlocoFromIndex :: Int -> TerrenoDLC
getBlocoFromIndex 0 = TerraDLC
getBlocoFromIndex 1 = AguaDLC
getBlocoFromIndex 2 = PedraDLC
getBlocoFromIndex 3 = ArDLC
getBlocoFromIndex 4 = Lava
getBlocoFromIndex _ = ArDLC




atualizaMapa :: MapaDLC -> Int -> Int -> TerrenoDLC -> MapaDLC
atualizaMapa mapa linha col novoBloco =
    let (antes, linhaAtual:depois) = splitAt linha mapa
        novaLinha = atualizaLinha linhaAtual col novoBloco
    in antes ++ [novaLinha] ++ depois

atualizaLinha :: [TerrenoDLC] -> Int -> TerrenoDLC -> [TerrenoDLC]
atualizaLinha linha col novoBloco =
    let (antes, _:depois) = splitAt col linha
    in antes ++ [novoBloco] ++ depois


-- | Verifica se a minhoca pode se mover (está no solo)
podeMover :: Estado -> NumMinhoca -> Bool
podeMover est i =
    case encontraIndiceLista i (minhocasEstado est) of
        Nothing -> False
        Just minhoca ->
            case posicaoMinhoca minhoca of
                Nothing -> False
                Just pos -> Tarefa2.estaNoSolo pos (mapaEstado est)

-- | Verifica se a minhoca pode se mover (está no solo)
podeMoverDLC :: EstadoDLC -> NumMinhoca -> Bool
podeMoverDLC est i =
    case encontraIndiceLista i (minhocasEstadoDLC est) of
        Nothing -> False
        Just minhoca ->
            case posicaoMinhocaDLC minhoca of
                Nothing -> False
                Just pos -> EfetuaJogada.estaNoSolo pos (mapaEstadoDLC est)


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
            if podeMoverDLC est i
            then let novoEst = EfetuaJogada.efetuaJogada i (DataDLC.Move dir) est
                 in (novoEst, DataDLC.Move dir)
            else (est, DataDLC.Move dir)


-- * DLC

adicionaObjetoDLC :: EstadoDLC -> Int -> Posicao -> Int -> EstadoDLC
adicionaObjetoDLC e idx pos secSel =
    let novoObjeto = case idx of
            0 -> BarrilDLC pos False
            1 -> HealthPack pos 50
            2 ->let ammoPacks = [JetpackDLC, EscavadoraDLC, BazucaDLC, MinaDLC, DinamiteDLC]
                    ammoType = ammoPacks !! (secSel `mod` 5)
                in AmmoPack pos 50 ammoType
            3 ->
                let disparos = [BazucaDLC, MinaDLC, DinamiteDLC, FlameTrower]
                    tipo = disparos !! (secSel `mod` 3)
                in DisparoDLC pos Norte tipo Nothing 0 -- ! isto tem valores defaults que tem de ser mudados
            _ -> BarrilDLC pos False
        objetosAtuais = objetosEstadoDLC e

        objetosFiltrados = filter (\obj -> DataDLC.posicaoObjeto obj /= pos) objetosAtuais
    in e { objetosEstadoDLC = objetosFiltrados ++ [novoObjeto] }

adicionaMinhocaDLC :: EstadoDLC -> Posicao -> EstadoDLC
adicionaMinhocaDLC e pos =
    let minhocasAtuais = minhocasEstadoDLC e

        minhocasFiltradas = filter (\m -> posicaoMinhocaDLC m /= Just pos) minhocasAtuais
        -- Cria nova minhoca
        novaMinhoca = MinhocaDLC {posicaoMinhocaDLC = Just pos, vidaMinhocaDLC = VivaDLC 100, jetpackMinhocaDLC = 100, escavadoraMinhocaDLC = 100, bazucaMinhocaDLC=100, minaMinhocaDLC = 100, dinamiteMinhocaDLC=100,flameMinhocaDLC = 100, burningCounter = 0, equipaMinhoca = Nothing}
    in e { minhocasEstadoDLC = minhocasFiltradas ++ [novaMinhoca] }  -- ! isto tem valores defaults que tem de ser mudados

removeObjetoDLC :: EstadoDLC -> Posicao -> EstadoDLC
removeObjetoDLC e pos =
    let objetosAtuais = objetosEstadoDLC e
        objetosFiltrados = filter (\obj -> DataDLC.posicaoObjeto obj /= pos) objetosAtuais
    in e { objetosEstadoDLC = objetosFiltrados }

removeMinhocaDLC :: EstadoDLC -> Posicao -> EstadoDLC
removeMinhocaDLC e pos =
    let minhocasAtuais = minhocasEstadoDLC e
        minhocasFiltradas = filter (\m -> posicaoMinhocaDLC m /= Just pos) minhocasAtuais
    in e { minhocasEstadoDLC = minhocasFiltradas }


encontraProximoIndiceValido :: Int -> [MinhocaDLC] -> Int
encontraProximoIndiceValido atual minhocas =
    let totalMinhocas = length minhocas

        indicesValidos = [i | (i, m) <- zip [0..] minhocas, eMinhocaVivaDLC m]
    in if null indicesValidos
       then 0
       else
           let
               proximosIndices = filter (> atual) indicesValidos
           in if null proximosIndices
              then head indicesValidos
              else head proximosIndices

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