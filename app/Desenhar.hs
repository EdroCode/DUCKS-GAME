{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
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
import DataDLC
import Auxiliar (getMinhocasValidasDLC, eMinhocaVivaDLC)
import EfetuaJogada
import AvancaEstado

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
desenha p (Menu sel) = return $ drawMenu p sel
desenha p (BotSimulation est _ _ (numMinhoca, jogada)) = return $ drawGame p est (Just numMinhoca) (Just jogada)
desenha p (PVP est _ _ jogada) = return $ drawPvPGame p est (Just jogada)
desenha p (MapCreatorTool e b a) = return $ (drawMCT p e b a) 
desenha p Quit = return $ Translate (-50) 0 $ Scale 0.5 0.5 $ Text "Aperte ESC para confirmar saída."
desenha p Help = return $ drawHelp

-- | Menu principal com seletor expandido
drawMenu :: [Picture] -> Int -> Picture
drawMenu p sel = Pictures
  [ 
    -- Título "WORMS"
    Translate (-120) 200 $ Scale 1 1 $ p !! 20
  , Translate (-280) 200 $ Scale 0.7 0.7 $ Color white $ Text "WORMS"
  
  , -- Subtítulo
    Translate (0 - 280) 130 $ Scale 0.2 0.2 $ Color (greyN 0.5) $ Text "Escolha o modo de jogo" 
  
  , -- Bot Simulation
    Translate (-280 - 120) (0) $ Scale 1 1 $ (if sel==0 then p !! 21 else p !! 22)
  , Translate (-340 - 280) (-15) $ Scale 0.35 0.35 $ Color (if sel==0 then red else black) $ Text "Bot Simulation"
  
  , -- Player vs Player
    Translate (280 - 120) (0) $ Scale 1 1 $ (if sel==1 then p !! 21 else p !! 22)
  , Translate (220 - 280) (-15) $ Scale 0.35 0.35 $ Color (if sel==1 then red else black) $ Text "Player vs Player"
  
  , -- MAP Creator Tool
    Translate (-280 - 120) (-130) $ Scale 1 1 $ (if sel==2 then p !! 21 else p !! 22)
  , Translate (-340 - 280) (-145) $ Scale 0.35 0.35 $ Color (if sel==2 then red else black) $ Text "MAP Creator Tool"
  
  , -- Help
    Translate (280 - 120) (-130) $ Scale 1 1 $ (if sel==3 then p !! 21 else p !! 22)
  , Translate (220 - 280) (-145) $ Scale 0.35 0.35 $ Color (if sel==3 then red else black) $ Text "Help"
  
  , -- Quit
    Translate (0 - 120) (-260) $ Scale 1 1 $ (if sel==4 then p !! 21 else p !! 22)
  , Translate (0 - 280) (-275) $ Scale 0.35 0.35 $ Color (if sel==4 then red else black) $ Text "Quit"
  ]

getDescription :: Int -> String
getDescription 0 = "Assista bots jogarem automaticamente"
getDescription 1 = "Modo local para dois jogadores"
getDescription 2 = "Crie os seus mapas"
getDescription 3 = "Veja os controles e instruções"
getDescription 4 = "Sair do jogo"
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


drawMCT :: [Picture] -> EstadoDLC -> Int -> Int -> Picture
drawMCT p e blocoSelecionado mode = Pictures
  [ Translate (-440) 330 $ Scale 0.5 0.5 $ Color black $ Text "Bem vindo ao criador de mapas", sidebar, world]
  where
    mapa = mapaEstadoDLC e
    objs = objetosEstadoDLC e
    ms = minhocasEstadoDLC e

    infoMapa = "Mapa: " ++ show (length mapa) ++ "x" ++ show (if null mapa then 0 else length (head mapa))
    
    blocos = [(0, "Terra", head p), (1, "Agua", p !! 1), (2, "Pedra", p !! 2), (3, "Ar", p !! 7), (4, "Lava", p !! 11) ]
    objetos =     [ (0, "Barril", p !! 5) , (1, "Health Pack", p !! 12) , (2, "Jetpack", p !! 15), (3, "Escavadora", p !! 16), (4, "Bazuca", p !! 17), (5, "Mina", p !! 18), (6, "Dinamite", p !! 19)]
    personagens = [(0, "Pato", p !! 3)]
    
    drawBloco y (idx, nome, pic) = Pictures
      [ -- Highlight 
        if idx == blocoSelecionado 
          then Color (makeColor 0.3 0.6 1.0 0.3) $ Translate (-750) y $ rectangleSolid 280 70
          else Blank
      , Translate (-880) y $ Scale 0.25 0.25 $ Color black $ Text (show (idx + 1))
      , Translate (-850) (y + 5) $ Scale 0.2 0.2 $ Color black $ Text nome
      , Translate (-700) y $ Scale 2 2 $ pic
      ]

    drawObjeto y (idx, nome, pic) = Pictures
      [ -- Highlight 
        if idx == blocoSelecionado 
          then Color (makeColor 0.3 0.6 1.0 0.3) $ Translate (-750) y $ rectangleSolid 280 70
          else Blank
      , Translate (-880) y $ Scale 0.25 0.25 $ Color black $ Text (show (idx + 1))
      , Translate (-850) (y + 5) $ Scale 0.2 0.2 $ Color black $ Text nome
      , Translate (-700) y $ Scale 2 2 $ pic
      ]

    drawPersonagens y (idx, nome, pic) = Pictures
      [ -- Highlight 
        if idx == blocoSelecionado 
          then Color (makeColor 0.3 0.6 1.0 0.3) $ Translate (-750) y $ rectangleSolid 280 70
          else Blank
      , Translate (-880) y $ Scale 0.25 0.25 $ Color black $ Text (show (idx + 1))
      , Translate (-850) (y + 5) $ Scale 0.2 0.2 $ Color black $ Text nome
      , Translate (-700) y $ Scale 2 2 $ pic
      ]
    
    
    sidebar = Pictures
      [ 
        Color (greyN 0.9) $ Translate (-750) 0 $ rectangleSolid 300 900
      
      , 
        Color white $ Translate (-750) 380 $ rectangleSolid 280 80
      , Translate (-870) 360 $ Scale 0.25 0.25 $ Color black $ Text infoMapa
      
      
      , 
        Color (greyN 0.7) $ Translate (-750) 320 $ rectangleSolid 280 2
      
      , 
        Translate (-900) 280 $ Scale 0.3 0.3 $ Color (greyN 0.3) $ Text "Blocos:"
      
      ,
      (case mode of 
          0 -> Pictures $ zipWith drawBloco [240, 150, 60, -30, -120] blocos
          1 -> Pictures $ zipWith drawObjeto [240, 150, 60, -30, -120, -210, -300] objetos  
          2 -> Pictures $ zipWith drawPersonagens [240] personagens)
    
      , 
        Translate (-900) (-400) $ Scale 0.2 0.2 $ Color (greyN 0.5) $ Text "L - Adicionar linha"
      , Translate (-900) (-430) $ Scale 0.2 0.2 $ Color (greyN 0.5) $ Text "C - Adicionar coluna"
      , Translate (-900) (-460) $ Scale 0.2 0.2 $ Color (greyN 0.5) $ Text "M - Remover coluna"
      , Translate (-900) (-490) $ Scale 0.2 0.2 $ Color (greyN 0.5) $ Text "N - Remover linha"
      , Translate (-900) (-520) $ Scale 0.2 0.2 $ Color (greyN 0.5) $ Text "1 - Selecionar bloco"
      , Translate (-900) (-550) $ Scale 0.2 0.2 $ Color (greyN 0.5) $ Text "LMB - Colocar bloco/objeto/personagem"
      , Translate (-900) (-580) $ Scale 0.2 0.2 $ Color (greyN 0.5) $ Text "RMB - Remover (Modo Selecionado)"


      ]
    

    linha = length mapa
    cols = if null mapa then 0 else length (head mapa)
    largura = fromIntegral cols * cellSize
    altura  = fromIntegral linha * cellSize
    sidebarWidth = 300
    usableWidth  = janelaLargura - sidebarWidth
    usableHeight = janelaAltura
    

    sx = if largura > 0 then usableWidth / largura else 1
    sy = if altura > 0 then usableHeight / altura else 1
    scaleFactor = min (min sx sy) 2.0  
    
    world =
      Translate (sidebarWidth / 2) 0 $
        Scale scaleFactor scaleFactor $
          Pictures
            [ drawMapaDLC p mapa
            , drawObjetosDLC p objs mapa
            , drawMinhocasStatic p ms mapa
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
    

    sx = if largura > 0 then usableWidth / largura else 1
    sy = if altura > 0 then usableHeight / altura else 1
    scaleFactor = min (min sx sy) 2.0  
    
    world =
      Translate (sidebarWidth / 2) 0 $
        Scale scaleFactor scaleFactor $
          Pictures
            [ drawMapa p mapa
            , drawObjetos p objs mapa
            , drawMinhocas p ms mapa numMinhoca jogada
            ]

drawPvPGame :: [Picture] -> EstadoDLC -> Maybe JogadaDLC -> Picture
drawPvPGame p est jogada =
  Pictures [sidebar, world]
  where
    mapa = mapaEstadoDLC est
    objs = objetosEstadoDLC est
    ms   = minhocasEstadoDLC est

    infoMapa =
      "mapa: " ++ show (length mapa) ++ "x" ++
      show (if null mapa then 0 else length (head mapa))

    minhocasVivas = length (getMinhocasValidasDLC ms)
    totalMinhocas = length ms
    totalObjetos  = length objs

    gameWindowPos = (-400)

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

    drawMinhocaBar :: Int -> MinhocaDLC -> Picture
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
                Text (show (vidaMinhocaDLC m))

          -- Posição
        , Translate (-700) (y - 40) $
            Scale 0.25 0.25 $
              Color blue $
                Text (show (posicaoMinhocaDLC m))

          -- Armas / itens
        , Translate (-900) (y - weaponOffset) $ Scale 1.5 1.5 $ p !! 2
        , Translate (-830) (y - weaponOffset) $ Scale 1.5 1.5 $ p !! 2
        , Translate (-760) (y - weaponOffset) $ Scale 1.5 1.5 $ p !! 6
        , Translate (-690) (y - weaponOffset) $ Scale 1.5 1.5 $ p !! 9
        , Translate (-620) (y - weaponOffset) $ Scale 1.5 1.5 $ p !! 8

        , Translate (-900) (y - weaponOffset) $
            Scale 0.25 0.25 $
              Color (if (armaSelecionada est == Just JetpackDLC && minhocaSelecionada est == i) then dark red else black) $
                Text (show (jetpackMinhocaDLC m))

        , Translate (-830) (y - weaponOffset) $
            Scale 0.25 0.25 $
              Color (if (armaSelecionada est == Just EscavadoraDLC && minhocaSelecionada est == i) then dark red else black) $
                Text (show (escavadoraMinhocaDLC m))

        , Translate (-760) (y - weaponOffset) $
            Scale 0.25 0.25 $
              Color (if (armaSelecionada est == Just BazucaDLC && minhocaSelecionada est == i) then dark red else black) $
                Text (show (bazucaMinhocaDLC m))

        , Translate (-690) (y - weaponOffset) $
            Scale 0.25 0.25 $
              Color (if (armaSelecionada est == Just MinaDLC && minhocaSelecionada est == i) then dark red else black) $
                Text (show (minaMinhocaDLC m))

        , Translate (-620) (y - weaponOffset) $
            Scale 0.25 0.25 $
              Color (if (armaSelecionada est == Just DinamiteDLC && minhocaSelecionada est == i) then dark red else black) $
                Text (show (dinamiteMinhocaDLC m))
        ]
      where
        y   = 55 - fromIntegral i * minhocaSpacing
        cor = if eMinhocaVivaDLC m then green else red

    linha = length mapa
    cols = if null mapa then 0 else length (head mapa)
    largura = fromIntegral cols * cellSize
    altura  = fromIntegral linha * cellSize
    sidebarWidth = 300
    usableWidth  = janelaLargura - sidebarWidth
    usableHeight = janelaAltura
    

    sx = if largura > 0 then usableWidth / largura else 1
    sy = if altura > 0 then usableHeight / altura else 1
    scaleFactor = min (min sx sy) 2.0  
    
    world =
      Translate 500 0 $
        Scale scaleFactor scaleFactor $
          Pictures
            [ drawMapaDLC p mapa
            , drawObjetosDLC p objs mapa
            , drawMinhocasDLC p ms mapa (Just 0) jogada
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

drawObjetos :: [Picture] -> [Objeto] -> Mapa -> Picture
drawObjetos p objs mapa = Pictures $ map drawO objs
  where
    drawO o@(Disparo {}) = Translate x y $ case tipoDisparo o of
      Bazuca -> bazucaDir p (direcaoDisparo o)
      Mina -> p !! 9
      Dinamite -> p !! 8
      _ -> Color black $ circleSolid (cellSize * 0.4)
      where (x,y) = converteMapa mapa (Tarefa0_2025.posicaoObjeto o)

    drawO b@(Barril {}) = Translate x y $ p !! 5
      where (x,y) = converteMapa mapa (Tarefa0_2025.posicaoObjeto b)
    



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

getSpriteParaAcao m (Just (Labs2025.Move dir)) p isActiveMinhoca mapa pos
  | isActiveMinhoca && not (Tarefa2.estaNoSolo pos mapa) = 
      if length p > 14 then p !! 14 else p !! 3  -- Caindo 
  | isActiveMinhoca && dir `elem` [Norte, Nordeste, Noroeste] = 
      if length p > 13 then p !! 13 else p !! 3  -- Pulando 
  | isActiveMinhoca = 
      if length p > 3 then p !! 3 else p !! 3  -- Andando 
  | otherwise = p !! 3  -- Idle 

getSpriteParaAcao m (Just (Labs2025.Dispara arma dir)) p isActiveMinhoca _ _
  | not isActiveMinhoca = p !! 3  -- Idle 
  | otherwise = case arma of
      Bazuca -> if length p > 2 then p !! 3 else p !! 3      
      Jetpack -> if length p > 2 then p !! 3 else p !! 3     
      Escavadora -> if length p > 2 then p !! 3 else p !! 3  
      Dinamite -> if length p > 2 then p !! 3 else p !! 3    
      Mina -> if length p > 2 then p !! 3 else p !! 3        

drawMinhocasStatic :: [Picture] -> [MinhocaDLC] -> MapaDLC -> Picture
drawMinhocasStatic p minhocas mapa = Pictures $ map drawM minhocas
  where
    drawM m = case posicaoMinhocaDLC m of
      Nothing -> Blank  
      Just pos -> 
        let (x, y) = converteMapaDLC mapa pos
            sprite = case vidaMinhocaDLC m of
              MortaDLC -> p !! 4 
              VivaDLC _ -> p !! 3
                    
        in Translate x y $ Pictures [sprite]
  








-- * DLC

drawMapaDLC :: [Picture] -> MapaDLC -> Picture
drawMapaDLC p mapa = Pictures $ concatMap drawRow (zip [0..] mapa)
  where
    drawRow (r, row) = map (drawTile r) (zip [0..] row)
    drawTile r (c, t) = Translate x y $ Pictures [colorTile t, Color (greyN 0.6) $ rectangleWire cellSize cellSize]
      where
        (x,y) = converteMapaDLC mapa (r,c)
        colorTile ArDLC = p !! 7
        colorTile AguaDLC = p !! 1
        colorTile TerraDLC | r > 0 && (mapa !! (r-1) !! c) == ArDLC = Scale 0.66 0.66 $ p !! 0
                        | otherwise = Scale 0.66 0.66 $ p !! 10
        colorTile PedraDLC = p !! 2
        colorTile Lava = p !! 11

drawObjetosDLC :: [Picture] -> [ObjetoDLC] -> MapaDLC -> Picture
drawObjetosDLC p objs mapa = Pictures $ map drawO objs
  where
    drawO o@(DisparoDLC {}) = Translate x y $ case tipoDisparoDLC o of
      BazucaDLC -> bazucaDir p (direcaoDisparoDLC o)
      MinaDLC -> p !! 9
      DinamiteDLC -> p !! 8
      _ -> Color black $ circleSolid (cellSize * 0.4)
      where (x,y) = converteMapaDLC mapa (DataDLC.posicaoObjeto o)

    drawO b@(BarrilDLC {}) = Translate x y $ p !! 5
      where (x,y) = converteMapaDLC mapa (DataDLC.posicaoObjeto b)
    
    drawO hp@(HealthPack {}) = Translate x y $ p !! 12
      where (x,y) = converteMapaDLC mapa (DataDLC.posicaoObjeto hp)
    
    drawO hp@(AmmoPack {}) = 
      case ammoType hp of
          JetpackDLC -> Translate x y $ p !! 15
          EscavadoraDLC -> Translate x y $ p !! 16
          BazucaDLC -> Translate x y $ p !! 17
          MinaDLC -> Translate x y $ p !! 18
          DinamiteDLC -> Translate x y $ p !! 19
      
      where (x,y) = converteMapaDLC mapa (DataDLC.posicaoObjeto hp)

getSpriteParaAcaoDLC :: MinhocaDLC -> Maybe JogadaDLC -> [Picture] -> Bool -> MapaDLC -> Posicao -> Picture
getSpriteParaAcaoDLC m Nothing p _ _ _ = p !! 3  -- Idle

getSpriteParaAcaoDLC m (Just (DataDLC.Move dir)) p isActiveMinhoca mapa pos
  | isActiveMinhoca && not (EfetuaJogada.estaNoSolo pos mapa) = 
      if length p > 14 then p !! 14 else p !! 3  -- Caindo 
  | isActiveMinhoca && dir `elem` [Norte, Nordeste, Noroeste] = 
      if length p > 13 then p !! 13 else p !! 3  -- Pulando 
  | isActiveMinhoca = 
      if length p > 3 then p !! 3 else p !! 3  -- Andando 
  | otherwise = p !! 3  -- Idle 

getSpriteParaAcaoDLC m (Just (DataDLC.Dispara arma dir)) p isActiveMinhoca _ _
  | not isActiveMinhoca = p !! 3  -- Idle 
  | otherwise = case arma of
      BazucaDLC -> if length p > 2 then p !! 3 else p !! 3      
      JetpackDLC -> if length p > 2 then p !! 3 else p !! 3     
      EscavadoraDLC -> if length p > 2 then p !! 3 else p !! 3  
      DinamiteDLC -> if length p > 2 then p !! 3 else p !! 3    
      MinaDLC -> if length p > 2 then p !! 3 else p !! 3    


converteMapaDLC :: MapaDLC -> Posicao -> (Float, Float)
converteMapaDLC mapa (r,c) = (x,y)
  where
    linha = length mapa
    cols = if null mapa then 0 else length (head mapa)
    largura = fromIntegral cols * cellSize
    altura = fromIntegral linha * cellSize
    left = - largura / 2 + cellSize / 2
    top = altura / 2 - cellSize / 2
    x = left + fromIntegral c * cellSize
    y = top - fromIntegral r * cellSize
    -- | Desenha as minhocas com sprites diferentes baseado na última jogada
drawMinhocasDLC :: [Picture] -> [MinhocaDLC] -> MapaDLC -> Maybe NumMinhoca -> Maybe JogadaDLC -> Picture
drawMinhocasDLC p ms mapa numMinhoca jogada = Pictures $ map drawM (zip [0..] ms)
  where
    drawM (i,m) = case posicaoMinhocaDLC m of
      Nothing -> Blank
      Just s -> Translate x y sprite
        where
          (x,y) = converteMapaDLC mapa s
          sprite = if vidaMinhocaDLC m == MortaDLC
            then p !! 4  -- Morto
            else getSpriteParaAcaoDLC m jogada p (Just i == numMinhoca) mapa s
