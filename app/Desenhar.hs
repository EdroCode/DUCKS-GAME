module Desenhar where

import Graphics.Gloss

import Worms
import Labs2025
import Tarefa0_geral
import Tarefa0_2025
import Tarefa2
import Tarefa3
import Tarefa4 (getMinhocasValidas)

janelaLargura :: Float
janelaLargura = 1000

janelaAltura :: Float
janelaAltura = 1000

-- * Assets

data Assets = Assets
  { imgWorm :: Picture
  , imgBarrel :: Picture
  , imgBackground :: Picture
  , grassImg :: Picture
  , waterImg :: Picture
  , stoneImg :: Picture
  }

type EstadoGloss = (Estado, Assets)

-- | Menu do jogo com múltiplas opções
desenha :: [Picture] -> Worms -> IO Picture
desenha p (Menu sel) = return $ drawMenu sel
desenha p (BotSimulation est _ _) = return $ drawGame p est
desenha p (FreeRoam est _ _ _) = return $ drawFreeRoamGame p est
desenha p Quit = return $ Translate (-50) 0 $ Scale 0.5 0.5 $ Text "Aperte ESC para confirmar saída."
desenha p Help = return $ drawHelp

-- | Menu principal com seletor expandido
drawMenu :: Int -> Picture
drawMenu sel = Pictures
  [ 
	
    Translate (-220) 180 $ Scale 0.7 0.7 $ Color black $ Text "WORMS"
  , Translate (-220) 130 $ Scale 0.2 0.2 $ Color (greyN 0.5) $ Text "Escolha o modo de jogo"
  , Translate (-220) 60 $ Scale 0.35 0.35 $ Color (if sel==0 then red else black) $ Text "Bot Simulation"
  , Translate (-220) (-20) $ Scale 0.35 0.35 $ Color (if sel==1 then red else black) $ Text "Player vs Player"
  , Translate (-220) (-120) $ Scale 0.35 0.35 $ Color (if sel==2 then red else black) $ Text "Help"
  , Translate (-220) (-160) $ Scale 0.35 0.35 $ Color (if sel==3 then red else black) $ Text "Quit"
  

  , Translate (-300) (-230) $ Scale 0.18 0.18 $ Color (greyN 0.4) $ Text (getDescription sel)
  ]

-- | Retorna a descrição de cada modo de jogo
getDescription :: Int -> String
getDescription 0 = "Assista bots jogarem automaticamente"
getDescription 1 = "Modo local para dois jogadores"
getDescription 2 = "Veja os controles e instruções"
getDescription 3 = "Sair do jogo"
getDescription _ = ""

-- | Tela de ajuda / instruções expandida
drawHelp :: Picture
drawHelp = Pictures
  [ Translate (-360) 280 $ Scale 0.5 0.5 $ Color black $ Text "Help / Instruções"
  

  , Translate (-360) 180 $ Scale 0.3 0.3 $ Color (greyN 0.3) $ Text "NAVEGAÇÃO"
  , Translate (-360) 140 $ Scale 0.25 0.25 $ Color black $ Text "↑/↓ - navegar no menu"
  , Translate (-360) 100 $ Scale 0.25 0.25 $ Color black $ Text "Enter - selecionar opção"
  , Translate (-360) 60 $ Scale 0.25 0.25 $ Color black $ Text "ESC - voltar/sair"
  
  , Translate (-360) (-220) $ Scale 0.18 0.18 $ Color (greyN 0.5) $ Text "Pressione ESC ou Enter para voltar ao menu"
  ]

cellSize :: Float
cellSize = 32






drawGame :: [Picture] -> Estado -> Picture
-- Mostra uma linha de status e, ao centro, o mundo (mapa + objetos + minhocas)
drawGame p est = Pictures [sidebar, world]
  where
    mapa = mapaEstado est
    infoMapa = "mapa: " ++ show (length mapa) ++ "x" ++ show (if null mapa then 0 else length (head mapa))
    objs = objetosEstado est
    ms = minhocasEstado est

    minhocasVivas = length (getMinhocasValidas ms)
    totalMinhocas = length ms
    totalObjetos = length objs

    sidebar = Pictures
      [ Color (greyN 0.9) $ Translate (-450) 0 $ rectangleSolid 300 900
      , Translate (-580) 300 $ Scale 0.18 0.18 $ Color black $ Text infoMapa
      , Translate (-580) 260 $ Scale 0.18 0.18 $ Color (dark green) $ Text ("Minhocas vivas: " ++ show minhocasVivas)
      , Translate (-580) 220 $ Scale 0.18 0.18 $ Color black $ Text ("Total minhocas: " ++ show totalMinhocas)
      , Translate (-580) 180 $ Scale 0.18 0.18 $ Color (dark red) $ Text ("Objetos: " ++ show totalObjetos)
      ]

   

    linha = length mapa
    cols = if null mapa then 0 else length (head mapa)
    largura = fromIntegral cols * cellSize
    altura  = fromIntegral linha * cellSize
    sidebarWidth = 300 

    usableWidth  = janelaLargura - sidebarWidth
    usableHeight = janelaAltura

    sx = usableWidth  / largura
    sy = usableHeight / altura
    scaleFactor = min sx sy

    world =
      Translate (sidebarWidth / 2) 0 $
        Scale scaleFactor scaleFactor $
          Pictures
            [ drawMapa p mapa
            , drawObjetos p objs mapa
            , drawMinhocas p ms mapa
            ]


drawFreeRoamGame :: [Picture] -> Estado -> Picture
drawFreeRoamGame p est = Pictures [sidebar, world]
  where
    mapa = mapaEstado est
    infoMapa = "mapa: " ++ show (length mapa) ++ "x" ++ show (if null mapa then 0 else length (head mapa))
    objs = objetosEstado est
    ms = minhocasEstado est

    minhocasVivas = length (getMinhocasValidas ms)
    totalMinhocas = length ms
    totalObjetos = length objs

    sidebar = Pictures
      [ Color (greyN 0.9) $ Translate (-450) 0 $ rectangleSolid 300 900
      , Translate (-580) 300 $ Scale 0.18 0.18 $ Color black $ Text infoMapa
      , Translate (-580) 260 $ Scale 0.18 0.18 $ Color (dark green) $ Text ("Minhocas vivas: " ++ show minhocasVivas)
      , Translate (-580) 220 $ Scale 0.18 0.18 $ Color black $ Text ("Total minhocas: " ++ show totalMinhocas)
      , Translate (-580) 180 $ Scale 0.18 0.18 $ Color (dark red) $ Text ("Objetos: " ++ show totalObjetos)
      ]

   

    linha = length mapa
    cols = if null mapa then 0 else length (head mapa)
    largura = fromIntegral cols * cellSize
    altura  = fromIntegral linha * cellSize
    sidebarWidth = 300 

    usableWidth  = janelaLargura - sidebarWidth
    usableHeight = janelaAltura

    sx = usableWidth  / largura
    sy = usableHeight / altura
    scaleFactor = min sx sy

    world =
      Translate (sidebarWidth / 2) 0 $
        Scale scaleFactor scaleFactor $
          Pictures
            [ drawMapa p mapa
            , drawObjetos p objs mapa
            , drawMinhocas p ms mapa
            ]

-- | Converte coordenadas do mapa (linha, coluna) para coordenadas em pixels
converteMapa :: Mapa -> Posicao -> (Float, Float)
converteMapa mapa (r,c) = (x,y)
  where
    linha = length mapa
    cols = if null mapa then 0 else length (head mapa)
    largura = fromIntegral cols * cellSize
    altura = fromIntegral linha * cellSize
    left = - largura / 2 + cellSize / 2
    top = altura / 2 - cellSize / 2
    x = left + fromIntegral c * cellSize
    y = top - fromIntegral r * cellSize

-- | Desenha todas as células do mapa
drawMapa :: [Picture] -> Mapa -> Picture
drawMapa p mapa = Pictures $ concatMap drawRow (zip [0..] mapa)
  where
    drawRow (r, row) = map (drawTile r) (zip [0..] row)
    drawTile r (c, t) = Translate x y $ Pictures [colorTile t, Color (greyN 0.6) $ rectangleWire cellSize cellSize]
      where
        (x,y) = converteMapa mapa (r,c)
        colorTile Ar = p !! 7
        colorTile Agua = p !! 1 
        colorTile Terra | r > 0 && (mapa !! (r-1) !! c) == Ar = p !! 0
                        | otherwise = p !! 10
        colorTile Pedra = p !! 2

-- | Desenha objetos no mapa
drawObjetos :: [Picture] -> [Objeto] -> Mapa -> Picture
drawObjetos p objs mapa = Pictures $ map drawO objs
  where
    drawO o@(Disparo {}) = Translate x y $ case tipoDisparo o of
      Bazuca -> bazucaDir p (direcaoDisparo o)
      Mina -> p !! 9
      Dinamite -> p !! 8
      _ -> Color black $ circleSolid (cellSize * 0.12)
      where (x,y) = converteMapa mapa (posicaoObjeto o)

    drawO b@(Barril {}) = Translate x y $ p !! 5
      where (x,y) = converteMapa mapa (posicaoObjeto b)

-- | Altera a direção da imagem da bazuca
bazucaDir :: [Picture] -> Direcao -> Picture
bazucaDir p dir = case dir of
    Este -> p !! 6
    Oeste -> Rotate 180 (p !! 6)
    Norte -> Rotate 90 (p !! 6)
    Sul -> Rotate 270 (p !! 6)
    Sudeste -> Rotate 45 (p !! 6)
    Sudoeste -> Rotate 135 (p !! 6)
    Nordeste -> Rotate 315 (p !! 6)
    Noroeste -> Rotate 225 (p !! 6)

-- | Desenha as minhocas
drawMinhocas :: [Picture] -> [Minhoca] -> Mapa -> Picture
drawMinhocas p ms mapa = Pictures $ map drawM (zip [0..] ms)
  where
    drawM (i,m) = case posicaoMinhoca m of
      Nothing -> Blank
      Just s -> Translate x y $ if vidaMinhoca m == Morta
        then p !! 4
        else p !! 3
        where
          (x,y) = converteMapa mapa s