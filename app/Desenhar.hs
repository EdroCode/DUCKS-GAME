{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Desenhar where

import Graphics.Gloss

import Worms
import Labs2025
import Tarefa0_2025
import Tarefa2
import Tarefa4 (getMinhocasValidas)
import DataDLC
import Auxiliar (getMinhocasValidasDLC, eMinhocaVivaDLC)
import EfetuaJogada
import Data.Char (toLower)



janelaLargura :: Float
janelaLargura = 1920
janelaAltura :: Float
janelaAltura = 1300

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
desenha p (MapCreatorTool e b a secSel thirdSel edit char worm) = return $ (drawMCT p e b a secSel thirdSel edit char worm)
desenha p (LevelSelector id estImp) = return $ (drawLvlSelector p id estImp)
desenha p Quit = return $ Translate (-50) 0 $ Scale 0.5 0.5 $ Text "Aperte ESC para confirmar saída."
desenha p Help = return $ drawHelp
desenha p (GameOver team) = return $ drawGameOver p team

-- | Menu principal com seletor expandido
drawMenu :: [Picture] -> Int -> Picture
drawMenu p sel = Pictures
  [
    Scale 0.8 0.8 $ p !! 85
    ,
    -- Título "WORMS"
    Translate (-120) 200 $ Scale 1 1 $ p !! 20
  , Translate (-280) 200 $ Scale 1.6 1.6 $ drawWord p "worms"

  , -- Subtítulo
    Translate (0 - 340) 130 $ Scale 0.5 0.5 $ drawWord p "Escolha o modo de jogo"

  , -- Bot Simulation
    Translate (-280 - 120) (0) $ Scale 1 1 $ (if sel==0 then p !! 21 else p !! 22)
  , Translate (-280 - 300) (0) $ Scale 0.6 0.6 $ Color (if sel==0 then red else black) $ drawWord p " Bot Simulation"

  , -- Player vs Player
    Translate (280 - 120) (0) $ Scale 1 1 $ (if sel==1 then p !! 21 else p !! 22)
  , Translate (220 - 240) (0) $ Scale 0.6 0.6 $ Color (if sel==1 then red else black) $ drawWord p  "Player vs Player"

  , -- MAP Creator Tool
    Translate (-280 - 120) (-130) $ Scale 1 1 $ (if sel==2 then p !! 21 else p !! 22)
  , Translate (-340 - 240) (-130) $ Scale 0.6 0.6 $ Color (if sel==2 then red else black) $ drawWord p  "MAP Creator Tool"

  , -- Help
    Translate (280 - 120) (-130) $ Scale 1 1 $ (if sel==3 then p !! 21 else p !! 22)
  , Translate (280 - 160) (-130) $ Scale 0.6 0.6 $ Color (if sel==3 then red else black) $ drawWord p  "Help"

  , -- Quit
    Translate (0 - 120) (-260) $ Scale 1 1 $ (if sel==4 then p !! 21 else p !! 22)
  , Translate (0 - 160) (-260) $ Scale 0.6 0.6 $ Color (if sel==4 then red else black) $ drawWord p  "Quit"
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

-- | Desenha o selector de níveis como uma lista vertical
drawLvlSelector :: [Picture] -> Int -> [EstadoDLC] -> Picture
drawLvlSelector p selected estadosImportados = Pictures
  [ Pictures $ zipWith drawNivel [0..] estadosImportados
  ]
  where
    drawNivel :: Int -> EstadoDLC -> Picture
    drawNivel idx estado = Pictures
      [ Color (if idx == selected then makeColor 0.3 0.6 1 0.5 else greyN 0.5) $
          Translate x y $ rectangleSolid largura altura
      , Translate (x - largura/4) y $ Scale 0.7 0.7 $ drawWord p ("Level " ++ show (idx + 1))
      , Translate (x + largura/4 - 100) y $ Scale 0.5 0.5 $ Color black $ drawWord p infoMapa
      ]
      where
        mapa = mapaEstadoDLC estado
        infoMapa = show (length mapa) ++ "x" ++ show (if null mapa then 0 else length (head mapa))
        largura = 1000
        altura  = 50
        espaco  = 10
        x = 0 + 20
        y = 300 - fromIntegral idx * (altura + espaco)


drawGameOver :: [Picture] -> Team -> Picture
drawGameOver p equipa =
    Pictures
        [Translate 0 100 $ Scale 0.8 0.8 $ Color (equipaCor equipa) $
            drawWord p (equipaStr equipa ++ " Ganha!")
        , Translate 0 (-50) $ Scale 0.4 0.4 $ Color (greyN 0.7) $
            drawWord p "Pressiona ESC para retornar ao menu"
        ]
  where
    equipaStr Red = "Equipa Vermelha"
    equipaStr Blue = "Equipa Azul"
    equipaCor Red = red
    equipaCor Blue = blue

drawMCT :: [Picture] -> EstadoDLC -> Int -> Int -> Int -> Int -> Bool -> Maybe Int -> MinhocaDLC -> Picture
drawMCT p e blocoSelecionado mode secSel thirdSel editMode char (MinhocaDLC pos vida jet esc baz mina dina flame burn equipa) = Pictures
  [ Translate (-440) 330 $ Scale 0.5 0.5 $ Color black $ drawWord p "Bem vindo ao criador de mapas", sidebar, world]
  where
    mapa = mapaEstadoDLC e
    objs = objetosEstadoDLC e
    ms = minhocasEstadoDLC e

    infoMapa = show (length mapa) ++ "x" ++ show (if null mapa then 0 else length (head mapa))

    blocos = [(0, "Terra", head p), (1, "Agua", p !! 1), (2, "Pedra", p !! 2), (3, "Ar", p !! 7), (4, "Lava", p !! 11) ]
    
    staticObjects = [(0, "Barril", p !! 5), (1, "Health Pack", p !! 12)]
    ammoPacks = [(2, "Jetpack", p !! 15), (2, "Escavadora", p !! 16), (2, "Bazuca", p !! 17), (2, "Mina", p !! 18), (2, "Dinamite", p !! 19)]
    disparos = [(3, "Bazuca", p !! 6), (3, "Mina", p !! 9), (3, "Dinamite", p !! 8), (3, "FireBall", p !! 69)]
    
    personagens = [(0, "Pato", p !! 3)]

    drawBloco y (idx, nome, pic) = Pictures
      [ if idx == blocoSelecionado
          then Color (makeColor 0.3 0.6 1.0 0.3) $ Translate (-750) y $ rectangleSolid 280 70
          else Blank
      , Translate (-880) y $ Scale 0.6 0.6 $ drawWord p (show (idx + 1))
      , Translate (-850) (y + 5) $ Scale 0.6 0.6 $ Color black $ drawWord p nome
      , Translate (-700) y $ Scale 2 2 $ pic
      ]

    drawObjeto y (idx, nome, pic) = Pictures
      [ if idx == blocoSelecionado
          then Color (makeColor 0.3 0.6 1.0 0.3) $ Translate (-750) y $ rectangleSolid 280 70
          else Blank
      , Translate (-880) y $ Scale 0.6 0.6 $ Color black $ drawWord p (show (idx + 1))
      , Translate (-850) (y + 5) $ Scale 0.6 0.6 $ Color black $ drawWord p nome
      , Translate (-700) y $ Scale 2 2 $ pic
      ]

    drawAmmoPackSelector y = Pictures
      [ if blocoSelecionado == 2
          then Color (makeColor 0.3 0.6 1.0 0.3) $ Translate (-750) y $ rectangleSolid 280 70
          else Blank
      , Translate (-880) y $ Scale 0.6 0.6 $ Color black $ drawWord p (show (selectedIdx + 1))
      , Translate (-850) (y + 5) $ Scale 0.6 0.6 $ Color black $ drawWord p "Ammo Pack"
      , Translate (-600) y $ Scale 2 2 $ selectedPic
      , Translate (-850) (y - 30) $ Scale 0.5 0.5 $ Color (greyN 0.5) $ drawWord p "<"
      , Translate (-650) (y - 30) $ Scale 0.5 0.5 $ Color (greyN 0.5) $ drawWord p ">"
      , Translate (-825) (y - 30) $ Scale 0.5 0.5 $ Color (greyN 0.5) $ drawWord p (selectedName)
      
      ]
      where
        (selectedIdx, selectedName, selectedPic) = if blocoSelecionado == 2 then ammoPacks !! secSel else head ammoPacks
    
    
    drawDisparoSelector y = Pictures
      [ if blocoSelecionado == 3
          then Color (makeColor 0.3 0.6 1.0 0.3) $ Translate (-750) y $ rectangleSolid 280 70
          else Blank
      , Translate (-880) y $ Scale 0.6 0.6 $ Color black $ drawWord p (show (selectedIdx + 1))
      , Translate (-850) (y + 5) $ Scale 0.6 0.6 $ Color black $ drawWord p "Disparos"
      , Translate (-600) y $ Scale 2 2 $ selectedPic
      , Translate (-850) (y - 30) $ Scale 0.5 0.5 $ Color (greyN 0.5) $ drawWord p "<"
      , Translate (-650) (y - 30) $ Scale 0.5 0.5 $ Color (greyN 0.5) $ drawWord p ">"
      , Translate (-825) (y - 30) $ Scale 0.5 0.5 $ Color (greyN 0.5) $ drawWord p selectedName
      ]
      where
        (selectedIdx, selectedName, selectedPic) = if blocoSelecionado == 3 then disparos !! secSel else head disparos

    weaponOffset :: Float
    weaponOffset = 80

    weaponTextOffset :: Float
    weaponTextOffset = 150

    drawPersonagens y (idx, nome, pic) = Pictures
      [ if idx == blocoSelecionado
          then Color (makeColor 0.3 0.6 1.0 0.3) $ Translate (-750) y $ rectangleSolid 280 70
          else Blank
      , Translate (-880) y $ Scale 0.6 0.6 $ Color black $ drawWord p (show (idx + 1))
      , Translate (-850) (y + 5) $ Scale 0.6 0.6 $ Color black $ drawWord p nome
      , Translate (-700) y $ Scale 2 2 $ pic

      , Translate (-900) (y - weaponOffset) $ Scale 1.5 1.5 $ p !! 2
      , Translate (-830) (y - weaponOffset) $ Scale 1.5 1.5 $ p !! 2
      , Translate (-760) (y - weaponOffset) $ Scale 1.5 1.5 $ p !! 6
      , Translate (-690) (y - weaponOffset) $ Scale 1.5 1.5 $ p !! 9
      , Translate (-620) (y - weaponOffset) $ Scale 1.5 1.5 $ p !! 8
      , Translate (-900) (y -  3*weaponOffset) $ Scale 1.5 1.5 $ p !! 69

      , Pictures
          [
            if thirdSel == 0
              then Color editColor $ 
                  Translate (-910) (y - weaponTextOffset) $ 
                  rectangleSolid 50 30
              else Blank
          , Translate (-920) (y - weaponTextOffset) $
              Scale 0.4 0.4 $
                Color black $
                  drawWord p (show jet)
          ]
          
      , Pictures
          [ 
            if thirdSel == 1
              then Color editColor $ 
                  Translate (-840) (y - weaponTextOffset) $ 
                  rectangleSolid 50 30
              else Blank
          , Translate (-830) (y - weaponTextOffset) $
              Scale 0.4 0.4 $
                Color black $
                  drawWord p (show esc)
          ]
          
      , Pictures
          [ 
            if thirdSel == 2
              then Color editColor $ 
                  Translate (-770) (y - weaponTextOffset) $ 
                  rectangleSolid 50 30
              else Blank
          , Translate (-760) (y - weaponTextOffset) $
              Scale 0.4 0.4 $
                Color black $
                  drawWord p (show baz)
          ]
          
      , Pictures
          [ 
            if thirdSel == 3
              then Color editColor $ 
                  Translate (-700) (y - weaponTextOffset) $ 
                  rectangleSolid 50 30
              else Blank
          , Translate (-690) (y - weaponTextOffset) $
              Scale 0.4 0.4 $
                Color black $
                  drawWord p (show mina)
          ]
          
      , Pictures
          [ 
            if thirdSel == 4
              then Color editColor $ 
                  Translate (-630) (y - weaponTextOffset) $ 
                  rectangleSolid 50 30
              else Blank
          , Translate (-620) (y - weaponTextOffset) $
              Scale 0.4 0.4 $
                Color black $
                  drawWord p (show dina)
          ]
          
                
      , Pictures
          [
            if thirdSel == 5
              then Color editColor $ 
                  Translate (-910) (y - weaponTextOffset - 140) $ 
                  rectangleSolid 50 30
              else Blank
          , Translate (-920) (y - weaponTextOffset - 140) $
              Scale 0.4 0.4 $
                Color black $
                  drawWord p (show flame)
          ]
          
      , Pictures
          [
            if thirdSel == 6
              then Color editColor $ 
                  Translate (-600) (y - weaponTextOffset - 140) $ 
                  rectangleSolid 50 30
              else Blank
          , Translate (-830) (y - weaponTextOffset - 140) $
              Scale 0.4 0.4 $
                Color black $
                  drawWord p ("Burning counter: " ++ show burn)
          ]
          
      , Pictures
        [ if thirdSel == 7
            then Color editColor $ 
              Translate (-800) (y - weaponTextOffset - 240) $ 
              rectangleSolid 50 30
            else Blank
        , Translate (-800) (y - weaponTextOffset - 240) $ Scale 0.6 0.6 $ drawWord p "Equipa:"
        
        ,Translate (-830) (y - weaponTextOffset - 240) $
            Scale 1 1 $
              case equipa of
                Just Red -> Color red $ rectangleSolid 20 20
                Just Blue -> Color blue $ rectangleSolid 20 20
                Nothing -> Color (greyN 0.5) $ rectangleSolid 20 20
        ]

          
          
          ]
        
    sidebar = Pictures
      [ Color (greyN 0.9) $ Translate (-750) 0 $ rectangleSolid 300 900
      , Color white $ Translate (-750) 380 $ rectangleSolid 280 80
      , Translate (-870) 360 $ Scale 1 1 $ Color black $ drawWord p infoMapa
      , Color (greyN 0.7) $ Translate (-750) 320 $ rectangleSolid 280 2
      , Translate (-900) 280 $ Scale 0.6 0.6 $ Color (greyN 0.3) $ drawWord p "Blocos:"
      
      , case mode of
          0 -> Pictures $ zipWith drawBloco [240, 150, 60, -30, -120] blocos
          1 -> Pictures $ 
                 zipWith drawObjeto [240, 150] staticObjects ++
                 [drawAmmoPackSelector 60] ++
                 [drawDisparoSelector (-50)]
          2 -> Pictures $ zipWith drawPersonagens [240] personagens

      , Translate (-900) (-330) $ Scale 0.6 0.6 $ Color (greyN 0.5) $ drawWord p "L - Adicionar linha"
      , Translate (-900) (-360) $ Scale 0.6 0.6 $ Color (greyN 0.5) $ drawWord p "C - Adicionar coluna"
      , Translate (-900) (-390) $ Scale 0.6 0.6 $ Color (greyN 0.5) $ drawWord p "M - Remover coluna"
      , Translate (-900) (-420) $ Scale 0.6 0.6 $ Color (greyN 0.5) $ drawWord p "N - Remover linha"
      , Translate (-900) (-450) $ Scale 0.6 0.6 $ Color (greyN 0.5) $ drawWord p "1 - Selecionar bloco"
      , Translate (-900) (-480) $ Scale 0.6 0.6 $ Color (greyN 0.5) $ drawWord p "LMB - Colocar bloco/objeto/personagem"
      , Translate (-900) (-510) $ Scale 0.6 0.6 $ Color (greyN 0.5) $ drawWord p "RMB - Remover (Modo Selecionado)"
      , Translate (-900) (-540) $ Scale 0.6 0.6 $ Color (greyN 0.5) $ drawWord p "E - Exportar estado"
      , Translate (-900) (-570) $ Scale 0.6 0.6 $ Color (greyN 0.5) $ drawWord p "< > - Navegar"
      , Translate (-900) (-600) $ Scale 0.6 0.6 $ Color (greyN 0.5) $ drawWord p "Enter - Editar valor"
      , Translate (-900) (-630) $ Scale 0.6 0.6 $ Color (greyN 0.5) $ drawWord p "Backspace/Delete - Eliminar valor"

      --, Translate (-200) (-630) $ Scale 0.6 0.6 $ Color (greyN 0.5) $ drawWord p ("1sel > " ++ show mode) -- ? debug
      --, Translate (0) (-630) $ Scale 0.6 0.6 $ Color (greyN 0.5) $ drawWord p ("-  2sel > " ++ show secSel) -- ? debug
      --, Translate (200) (-630) $ Scale 0.6 0.6 $ Color (greyN 0.5) $ drawWord p ("-  3sel > " ++ show thirdSel) -- ? debug
      --, Translate (400) (-630) $ Scale 0.6 0.6 $ Color (greyN 0.5) $ drawWord p ("-  io > " ++ show char) -- ? debug

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

    editColor = if editMode == False then makeColor 0.3 0.6 1.0 0.3 else red

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
      , Translate (-900) 300 $ Scale 0.3 0.3 $ Color black $ drawWord p infoMapa
      , Translate (-900) 260 $ Scale 0.3 0.3 $ Color (dark green) $ drawWord p ("Minhocas vivas: " ++ show minhocasVivas)
      , Translate (-900) 220 $ Scale 0.3 0.3 $ Color (dark red) $ drawWord p ("Minhocas mortas: " ++ show (totalMinhocas - minhocasVivas))
      , Translate (-900) 180 $ Scale 0.3 0.3 $ Color black $ drawWord p ("Total minhocas: " ++ show totalMinhocas)
      , Translate (-900) 140 $ Scale 0.3 0.3 $ Color (dark red) $ drawWord p ("Objetos: " ++ show totalObjetos)
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
          , Translate (-900) 300 $ Scale 0.6 0.6 $ Color black $ drawWord p infoMapa
          , Translate (-900) 260 $ Scale 0.6 0.6 $ Color (dark green)
              $ drawWord p ("Minhocas vivas: " ++ show minhocasVivas)
          , Translate (-900) 220 $ Scale 0.6 0.6 $ Color (dark red)
              $ drawWord p ("Minhocas mortas: " ++ show (totalMinhocas - minhocasVivas))
          , Translate (-900) 180 $ Scale 0.6 0.6 $ Color black
              $ drawWord p ("Total minhocas: " ++ show totalMinhocas)
          , Translate (-900) 140 $ Scale 0.6 0.6 $ Color black
              $ drawWord p ("Objetos: " ++ show totalObjetos)
          , Translate (-900) 110 $ Scale 0.6 0.6 $ Color black
              $ drawWord p "---------"
          ]
          ++ minhocasSidebar
        )


    minhocasSidebar :: [Picture]
    minhocasSidebar =
      zipWith drawMinhocaBar [0 ..] ms

    minhocaSpacing :: Float
    minhocaSpacing = 240

    weaponOffset :: Float
    weaponOffset = 120

    drawMinhocaBar :: Int -> MinhocaDLC -> Picture
    drawMinhocaBar i m =
      Pictures
          [ -- Nome / índice
            Translate (-800) y $
            Scale 0.6 0.6 $
            Color (if (armaSelecionada est == Nothing && minhocaSelecionada est == i) then dark red else black) $
                  drawWord p (show i)
          -- Ícone vida
          , Translate (-900) (y - 20) $ Scale 2.5 2.5 $ p !! 3
          , Translate (-800) (y - 30) $
            Scale 0.6 0.6 $
            drawWord p ("HP: " ++ extrairVida (show (vidaMinhocaDLC m)))
          -- Equipa
          , case equipaMinhoca m of
              Just Red -> Translate (-600) (y - 30) $ Color red $ rectangleSolid 20 20
              Just Blue -> Translate (-600) (y - 30) $ Color blue $ rectangleSolid 20 20
              Nothing -> Blank
          -- Posição
          , Translate (-800) (y - 60) $
            Scale 0.6 0.6 $
            drawWord p ("POS: " ++ extrairPosicao (show (posicaoMinhocaDLC m)))
          -- Armas / itens (imagens)
          , Translate (-930) (y - weaponOffset) $ Scale 1.5 1.5 $ p !! 2
          , Translate (-840) (y - weaponOffset) $ Scale 1.5 1.5 $ p !! 2
          , Translate (-750) (y - weaponOffset) $ Scale 1.5 1.5 $ p !! 6
          , Translate (-660) (y - weaponOffset) $ Scale 1.5 1.5 $ p !! 9
          , Translate (-570) (y - weaponOffset) $ Scale 1.5 1.5 $ p !! 8
          -- Barras divisórias verticais
          , Translate (-885) (y - weaponOffset - 10) $
            Color (greyN 0.3) $
            rectangleSolid 2 60
          , Translate (-795) (y - weaponOffset - 10) $
            Color (greyN 0.3) $
            rectangleSolid 2 60
          , Translate (-705) (y - weaponOffset - 10) $
            Color (greyN 0.3) $
            rectangleSolid 2 60
          , Translate (-615) (y - weaponOffset - 10) $
            Color (greyN 0.3) $
            rectangleSolid 2 60
          -- Textos das armas (abaixo das imagens)
          , Translate (-938) (y - weaponOffset - 60) $
            Scale 0.6 0.6 $
            Color (if armaSelecionada est == Just JetpackDLC && minhocaSelecionada est == i then dark red else black) $
                  drawWord p (show (jetpackMinhocaDLC m) ++ " ")
          , Translate (-848) (y - weaponOffset - 60) $
            Scale 0.6 0.6 $
            Color (if armaSelecionada est == Just EscavadoraDLC && minhocaSelecionada est == i then dark red else black) $
                  drawWord p (show (escavadoraMinhocaDLC m) ++ " ")
          , Translate (-758) (y - weaponOffset - 60) $
            Scale 0.6 0.6 $
            Color (if armaSelecionada est == Just BazucaDLC && minhocaSelecionada est == i then dark red else black) $
                  drawWord p (show (bazucaMinhocaDLC m) ++ " ")
          , Translate (-668) (y - weaponOffset - 60) $
            Scale 0.6 0.6 $
            Color (if armaSelecionada est == Just MinaDLC && minhocaSelecionada est == i then dark red else black) $
                  drawWord p (show (minaMinhocaDLC m) ++ " ")
          , Translate (-578) (y - weaponOffset - 60) $
            Scale 0.6 0.6 $
            Color (if armaSelecionada est == Just DinamiteDLC && minhocaSelecionada est == i then dark red else black) $
                  drawWord p (show (dinamiteMinhocaDLC m) ++ " ")
          -- Barra horizontal divisória (no final de cada personagem)
          , Translate (-750) (y - weaponOffset - 90) $
            Color (greyN 0.4) $
            rectangleSolid 400 3
          ]
        where
          y   = 55 - fromIntegral i * minhocaSpacing

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


drawWord :: [Picture] -> String -> Picture
drawWord p str =
  Pictures $
    zipWith (\x pic -> Translate x 0 pic) xs (stringToPictures p str)
  where
    spacing = 40
    xs = [0, spacing ..]



stringToPictures :: [Picture] -> String -> [Picture]
stringToPictures p str =
  map (drawLetters p) str


drawLetters :: [Picture] -> Char -> Picture
drawLetters p c =
  let lc = toLower c in
  case lc of
    'a' -> p !! 23
    'b' -> p !! 24
    'c' -> p !! 25
    'd' -> p !! 26
    'e' -> p !! 27
    'f' -> p !! 28
    'g' -> p !! 29
    'h' -> p !! 30
    'i' -> p !! 31
    'j' -> p !! 32
    'k' -> p !! 33
    'l' -> p !! 34
    'm' -> p !! 35
    'n' -> p !! 36
    'o' -> p !! 37
    'p' -> p !! 38
    'q' -> p !! 39
    'r' -> p !! 40
    's' -> p !! 41
    't' -> p !! 42
    'u' -> p !! 43
    'v' -> p !! 44
    'w' -> p !! 45
    'x' -> p !! 46
    'y' -> p !! 47
    'z' -> p !! 48
    ' '   -> p !! 49
    '0' -> p !! 50
    '1' -> p !! 51
    '2' -> p !! 52
    '3' -> p !! 53
    '4' -> p !! 54
    '5' -> p !! 55
    '6' -> p !! 56
    '7' -> p !! 57
    '8' -> p !! 58
    '9' -> p !! 59
    '-' -> p !! 60
    '(' -> p !! 61
    ')' -> p !! 62
    '/' -> p !! 63
    ':' -> p !! 64
    ',' -> p !! 65
    '!' -> p !! 66
    '?' -> p !! 67
    '@' -> p !! 68
    '<' -> p !! 70
    '>' -> p !! 71









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
getSpriteParaAcao _ Nothing p _ _ _ = p !! 3  -- Idle

getSpriteParaAcao _ (Just (Labs2025.Move dir)) p isActiveMinhoca mapa pos
  | isActiveMinhoca && not (Tarefa2.estaNoSolo pos mapa) =
      if length p > 14 then p !! 14 else p !! 3  -- Caindo 
  | isActiveMinhoca && dir `elem` [Norte, Nordeste, Noroeste] =
      if length p > 13 then p !! 13 else p !! 3  -- Pulando 
  | isActiveMinhoca =
      if length p > 3 then p !! 3 else p !! 3  -- Andando 
  | otherwise = p !! 3  -- Idle 

getSpriteParaAcao _ (Just (Labs2025.Dispara arma _)) p isActiveMinhoca _ _
  | not isActiveMinhoca = p !! 3  -- Idle 
  | otherwise = case arma of
      Bazuca -> p !! 3
      Jetpack ->  p !! 3
      Escavadora -> p !! 3
      Dinamite -> p !! 3
      Mina -> p !! 3






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
      FlameTrower -> p !! 69
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
getSpriteParaAcaoDLC _ Nothing p _ _ _ = p !! 3

getSpriteParaAcaoDLC minhoca (Just (DataDLC.Move dir)) p isActiveMinhoca mapa pos
  | burningCounter minhoca > 0 = p !! 84  
  | isActiveMinhoca && not (EfetuaJogada.estaNoSolo pos mapa) =
      case equipaMinhoca minhoca of
        Just Red -> p !! 75  
        Just Blue -> p !! 74  
        Nothing -> p !! 14
  | isActiveMinhoca && dir `elem` [Norte, Nordeste, Noroeste] =
      case equipaMinhoca minhoca of
        Just Red -> p !! 77  
        Just Blue -> p !! 76 
        Nothing -> p !! 13
  | otherwise = 
      case equipaMinhoca minhoca of
        Just Red -> if burningCounter minhoca > 0 then p !! 84 else p !! 81
        Just Blue -> if burningCounter minhoca > 0 then p !! 84 else p !! 80
        Nothing -> p !! 3

getSpriteParaAcaoDLC minhoca (Just (DataDLC.Dispara arma _)) p isActiveMinhoca _ _
  | burningCounter minhoca > 0 = p !! 84  
  | not isActiveMinhoca = 
      case equipaMinhoca minhoca of
        Just Red -> if vidaMinhocaDLC minhoca >= VivaDLC 50 then p !! 79 else p !! 82
        Just Blue -> if vidaMinhocaDLC minhoca >= VivaDLC 50 then p !! 80 else p !! 81
        Nothing -> p !! 3
  | otherwise = case arma of
      BazucaDLC -> case equipaMinhoca minhoca of
        Just Red -> p !! 72  
        Just Blue -> p !! 73  
        Nothing -> p !! 3
      JetpackDLC -> case equipaMinhoca minhoca of
        Just Red -> p !! 82
        Just Blue -> p !! 81
        Nothing -> p !! 3
      EscavadoraDLC -> case equipaMinhoca minhoca of
        Just Red -> p !! 83  
        Just Blue -> p !! 84  
        Nothing -> p !! 3
      DinamiteDLC -> case equipaMinhoca minhoca of
        Just Red -> p !! 82
        Just Blue -> p !! 81
        Nothing -> p !! 3
      MinaDLC -> case equipaMinhoca minhoca of
        Just Red -> p !! 82
        Just Blue -> p !! 81
        Nothing -> p !! 3


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

-- Função para extrair apenas o valor da vida
extrairVida :: String -> String
extrairVida str = case words str of
    ["VivaDLC", hp] -> hp
    ["MortaDLC", hp] -> hp
    _ -> "N/A"

-- Função para extrair a posição
extrairPosicao :: String -> String
extrairPosicao str = case words str of
    ["Just", pos] -> pos
    ["Nothing"] -> "N/A"
    _ -> "N/A"