module Desenhar where

import Graphics.Gloss

import Worms
import Labs2025
import Tarefa0_geral
import Tarefa0_2025
import Tarefa1
import Tarefa2
import Tarefa3
import Tarefa4 (getMinhocasValidas)
import Data.Semigroup (Min(Min))
import Data.Data (ConstrRep(FloatConstr))



janelaLargura :: Float
janelaLargura = 1900
janelaAltura :: Float
janelaAltura = 1900

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
desenha p (BotSimulation est _ _ (numMinhoca, jogada)) = return $ drawGame p est (Just numMinhoca) (Just jogada)
desenha p (FreeRoam est _ _ jogada) = return $ drawFreeRoamGame p est (Just jogada)
desenha p Quit = return $ Translate (-50) 0 $ Scale 0.5 0.5 $ Text "Aperte ESC para confirmar saída."
desenha p Help = return $ drawHelp

-- | Menu principal com seletor expandido
drawMenu :: Int -> Picture
drawMenu sel = Pictures
  [ Translate (-220) 180 $ Scale 0.7 0.7 $ Color black $ Text "WORMS"
  , Translate (-220) 130 $ Scale 0.2 0.2 $ Color (greyN 0.5) $ Text "Escolha o modo de jogo"
  , Translate (-220) 60 $ Scale 0.35 0.35 $ Color (if sel==0 then red else black) $ Text "Bot Simulation"
  , Translate (-220) (-20) $ Scale 0.35 0.35 $ Color (if sel==1 then red else black) $ Text "Player vs Player"
  , Translate (-220) (-120) $ Scale 0.35 0.35 $ Color (if sel==2 then red else black) $ Text "Help"
  , Translate (-220) (-160) $ Scale 0.35 0.35 $ Color (if sel==3 then red else black) $ Text "Quit"
  , Translate (-300) (-230) $ Scale 0.10 0.10 $ Color (greyN 0.4) $ Text (getDescription sel)
  ]

getDescription :: Int -> String
getDescription 0 = "Assista bots jogarem automaticamente"
getDescription 1 = "Modo local para dois jogadores"
getDescription 2 = "Veja os controles e instruções"
getDescription 3 = "Sair do jogo"
getDescription _ = ""

drawHelp :: Picture
drawHelp = Pictures
  [ Translate (-360) 280 $ Scale 0.5 0.5 $ Color black $ Text "Help / Instruções"
  , Translate (-360) 180 $ Scale 0.3 0.3 $ Color (greyN 0.3) $ Text "NAVEGAÇÃO"
  , Translate (-360) 140 $ Scale 0.25 0.25 $ Color black $ Text "↑/↓ - navegar no menu"
  , Translate (-360) 100 $ Scale 0.25 0.25 $ Color black $ Text "Enter - selecionar opção"
  , Translate (-360) 60 $ Scale 0.25 0.25 $ Color black $ Text "ESC - voltar/sair"
  , Translate (-360) (-20) $ Scale 0.3 0.3 $ Color (greyN 0.3) $ Text "CONTROLES NO JOGO"
  , Translate (-360) (-60) $ Scale 0.25 0.25 $ Color black $ Text "Setas - mover minhoca"
  , Translate (-360) (-100) $ Scale 0.25 0.25 $ Color black $ Text "Q/E/Z/C - diagonais"
  , Translate (-360) (-220) $ Scale 0.10 0.10 $ Color (greyN 0.5) $ Text "Pressione ESC ou Enter para voltar ao menu"
  ]

cellSize :: Float
cellSize = 32

drawGame :: [Picture] -> Estado -> Maybe NumMinhoca -> Maybe Jogada -> Picture
drawGame p est numMinhoca jogada = Pictures [sidebar, world]
  where
    mapa = mapaEstado est
    infoMapa = "mapa: " ++ show (length mapa) ++ "x" ++ show (if null mapa then 0 else length (head mapa))
    objs = objetosEstado est
    ms = minhocasEstado est

    minhocasVivas = length (getMinhocasValidas ms)
    totalMinhocas = length ms
    totalObjetos = length objs

    sidebar = Pictures
      [ Color (greyN 0.9) $ Translate (-750) 0 $ rectangleSolid 300 900
      , Translate (-900) 300 $ Scale 0.3 0.3 $ Color black $ Text infoMapa
      , Translate (-900) 260 $ Scale 0.3 0.3 $ Color (dark green) $ Text ("Minhocas vivas: " ++ show minhocasVivas)
      , Translate (-900) 220 $ Scale 0.3 0.3 $ Color (dark red) $ Text ("Minhocas mortas: " ++ show (totalMinhocas - minhocasVivas))
      , Translate (-900) 180 $ Scale 0.3 0.3 $ Color black $ Text ("Total minhocas: " ++ show totalMinhocas)
      , Translate (-900) 140 $ Scale 0.3 0.3 $ Color (dark red) $ Text ("Objetos: " ++ show totalObjetos)
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
            , drawMinhocas p ms mapa numMinhoca jogada
            ]

drawFreeRoamGame :: [Picture] -> Estado -> Maybe Jogada -> Picture
drawFreeRoamGame p est jogada =
  Pictures [sidebar, world]
  where
    mapa = mapaEstado est
    objs = objetosEstado est
    ms   = minhocasEstado est

    infoMapa =
      "mapa: " ++ show (length mapa) ++ "x" ++
      show (if null mapa then 0 else length (head mapa))

    minhocasVivas = length (getMinhocasValidas ms)
    totalMinhocas = length ms
    totalObjetos  = length objs


    sidebar =
      Pictures
        ( [ Color (greyN 0.9)
              $ Translate (-750) 0
              $ rectangleSolid (1000 - gameWindowPos) 900
          , Translate (-900) 300 $ Scale 0.3 0.3 $ Color black $ Text infoMapa
          , Translate (-900) 260 $ Scale 0.3 0.3 $ Color (dark green)
              $ Text ("Minhocas vivas: " ++ show minhocasVivas)
          , Translate (-900) 220 $ Scale 0.3 0.3 $ Color (dark red)
              $ Text ("Minhocas mortas: " ++ show (totalMinhocas - minhocasVivas))
          , Translate (-900) 180 $ Scale 0.3 0.3 $ Color black
              $ Text ("Total minhocas: " ++ show totalMinhocas)
          , Translate (-900) 140 $ Scale 0.3 0.3 $ Color black
              $ Text ("Objetos: " ++ show totalObjetos)
          , Translate (-900) 110 $ Scale 0.3 0.3 $ Color black
              $ Text "---------"
          ]
          ++ minhocasSidebar
        )


    minhocasSidebar :: [Picture]
    minhocasSidebar =
      zipWith drawMinhocaBar [0 ..] ms


    minhocaSpacing :: Float
    minhocaSpacing = 150

    weaponOffset :: Float
    weaponOffset = 80

    drawMinhocaBar :: Int -> Minhoca -> Picture
    drawMinhocaBar i m =
      Pictures
        [ -- Nome / índice
          Translate (-900) y $
            Scale 0.25 0.25 $
              Color (if (armaSelecionada est == Nothing && minhocaSelecionada est == i) then dark red else black) $
                Text (show i)

          -- Ícone vida
        , Translate (-900) (y - 20) $ Scale 3 3 $ p !! 3
        , Translate (-860) (y - 30) $
            Scale 0.25 0.25 $
              Color cor $
                Text (show (vidaMinhoca m))

          -- Posição
        , Translate (-700) (y - 40) $
            Scale 0.25 0.25 $
              Color blue $
                Text (show (posicaoMinhoca m))

          -- Armas / itens
        , Translate (-900) (y - weaponOffset) $ Scale 1.5 1.5 $ p !! 2
        , Translate (-830) (y - weaponOffset) $ Scale 1.5 1.5 $ p !! 2
        , Translate (-760) (y - weaponOffset) $ Scale 1.5 1.5 $ p !! 6
        , Translate (-690) (y - weaponOffset) $ Scale 1.5 1.5 $ p !! 9
        , Translate (-620) (y - weaponOffset) $ Scale 1.5 1.5 $ p !! 8

        , Translate (-900) (y - weaponOffset) $
            Scale 0.25 0.25 $
              Color (if (armaSelecionada est == Just Jetpack && minhocaSelecionada est == i) then dark red else black) $
                Text (show (jetpackMinhoca m))

        , Translate (-830) (y - weaponOffset) $
            Scale 0.25 0.25 $
              Color (if (armaSelecionada est == Just Escavadora && minhocaSelecionada est == i) then dark red else black) $
                Text (show (escavadoraMinhoca m))

        , Translate (-760) (y - weaponOffset) $
            Scale 0.25 0.25 $
              Color (if (armaSelecionada est == Just Bazuca && minhocaSelecionada est == i) then dark red else black) $
                Text (show (bazucaMinhoca m))

        , Translate (-690) (y - weaponOffset) $
            Scale 0.25 0.25 $
              Color (if (armaSelecionada est == Just Mina && minhocaSelecionada est == i) then dark red else black) $
                Text (show (minaMinhoca m))

        , Translate (-620) (y - weaponOffset) $
            Scale 0.25 0.25 $
              Color (if (armaSelecionada est == Just Dinamite && minhocaSelecionada est == i) then dark red else black) $
                Text (show (dinamiteMinhoca m))
        ]
      where
        y   = 55 - fromIntegral i * minhocaSpacing
        cor = if eMinhocaViva m then green else red


    linha   = length mapa
    cols    = if null mapa then 0 else length (head mapa)
    largura = fromIntegral cols * cellSize
    altura  = fromIntegral linha * cellSize

    sidebarWidth = 300
    usableWidth  = janelaLargura - sidebarWidth
    usableHeight = janelaAltura

    sx = usableWidth / largura
    sy = usableHeight / altura
    scaleFactor = min sx sy

    gameWindowPos = 500

    world =
      Translate gameWindowPos 0 $
        Scale scaleFactor scaleFactor $
          Pictures
            [ drawMapa p mapa
            , drawObjetos p objs mapa
            , drawMinhocas p ms mapa (Just 0) jogada
            ]


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

drawMapa :: [Picture] -> Mapa -> Picture
drawMapa p mapa = Pictures $ concatMap drawRow (zip [0..] mapa)
  where
    drawRow (r, row) = map (drawTile r) (zip [0..] row)
    drawTile r (c, t) = Translate x y $ Pictures [colorTile t, Color (greyN 0.6) $ rectangleWire cellSize cellSize]
      where
        (x,y) = converteMapa mapa (r,c)
        colorTile Ar = p !! 7
        colorTile Agua = p !! 1
        colorTile Terra | r > 0 && (mapa !! (r-1) !! c) == Ar = Scale 0.66 0.66 $ p !! 0
                        | otherwise = Scale 0.66 0.66 $ p !! 10
        colorTile Pedra = p !! 2
        colorTile Lava = p !! 11

drawObjetos :: [Picture] -> [Objeto] -> Mapa -> Picture
drawObjetos p objs mapa = Pictures $ map drawO objs
  where
    drawO o@(Disparo {}) = Translate x y $ case tipoDisparo o of
      Bazuca -> bazucaDir p (direcaoDisparo o)
      Mina -> p !! 9
      Dinamite -> p !! 8
      _ -> Color black $ circleSolid (cellSize * 0.4)
      where (x,y) = converteMapa mapa (posicaoObjeto o)

    drawO b@(Barril {}) = Translate x y $ p !! 5
      where (x,y) = converteMapa mapa (posicaoObjeto b)
    
    drawO hp@(HealthPack {}) = Translate x y $ p !! 12
      where (x,y) = converteMapa mapa (posicaoObjeto hp)

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

-- | Desenha as minhocas com sprites diferentes baseado na última jogada
drawMinhocas :: [Picture] -> [Minhoca] -> Mapa -> Maybe NumMinhoca -> Maybe Jogada -> Picture
drawMinhocas p ms mapa numMinhoca jogada = Pictures $ map drawM (zip [0..] ms)
  where
    drawM (i,m) = case posicaoMinhoca m of
      Nothing -> Blank
      Just s -> Translate x y sprite
        where
          (x,y) = converteMapa mapa s
          sprite = if vidaMinhoca m == Morta
            then p !! 4  -- Morto
            else getSpriteParaAcao m jogada p (Just i == numMinhoca) mapa s

{- Retorna o sprite correto baseado na ação
 Índices dos sprites na lista:
 0: grass, 1: water, 2: stone, 3: worm (idle), 4: morto, 5: barril,
 6: bazuca, 7: sky, 8: dinamite, 9: mina, 10: dirt,
 11: worm andando, 12: worm pulando, 13: worm caindo,
 14: worm bazuca, 15: worm jetpack, 16: worm escavadora,
 17: worm dinamite, 18: worm mina
-}
getSpriteParaAcao :: Minhoca -> Maybe Jogada -> [Picture] -> Bool -> Mapa -> Posicao -> Picture
getSpriteParaAcao m Nothing p _ _ _ = p !! 3  -- Idle

getSpriteParaAcao m (Just (Move dir)) p isActiveMinhoca mapa pos
  | isActiveMinhoca && not (estaNoSolo pos mapa) = 
      if length p > 14 then p !! 14 else p !! 3  -- Caindo 
  | isActiveMinhoca && dir `elem` [Norte, Nordeste, Noroeste] = 
      if length p > 13 then p !! 13 else p !! 3  -- Pulando 
  | isActiveMinhoca = 
      if length p > 3 then p !! 3 else p !! 3  -- Andando 
  | otherwise = p !! 3  -- Idle 

getSpriteParaAcao m (Just (Dispara arma dir)) p isActiveMinhoca _ _
  | not isActiveMinhoca = p !! 3  -- Idle 
  | otherwise = case arma of
      Bazuca -> if length p > 2 then p !! 3 else p !! 3      
      Jetpack -> if length p > 2 then p !! 3 else p !! 3     
      Escavadora -> if length p > 2 then p !! 3 else p !! 3  
      Dinamite -> if length p > 2 then p !! 3 else p !! 3    
      Mina -> if length p > 2 then p !! 3 else p !! 3        
