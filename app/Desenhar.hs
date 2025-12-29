{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Desenhar where

import Graphics.Gloss

import Worms
import Labs2025
import Tarefa0_2025
import Tarefa2
import Tarefa4 (getMinhocasValidas, getXWay)
import DataDLC
import Auxiliar (getMinhocasValidasDLC, eMinhocaVivaDLC)
import EfetuaJogada
import Data.Char (toLower)



janelaLargura :: Float
janelaLargura = 1920
janelaAltura :: Float
janelaAltura = 1080


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
desenha p (PVP est _ _ jogada) = return $ drawPvPGame p est jogada
desenha p (MapCreatorTool e b a secSel thirdSel edit char worm) = return $ (drawMCT p e b a secSel thirdSel edit char worm)
desenha p (LevelSelector i estImp) = return $ (drawLvlSelector p i estImp)
desenha p (Quit sel) = return $ drawQuitConfirm p sel
desenha p (Help pagina) = return $ drawHelp p pagina
desenha p (GameOver team) = return $ drawGameOver p team

-- | UI for quit confirmation with two buttons (Confirmar / Cancelar)
drawQuitConfirm :: [Picture] -> Int -> Picture
drawQuitConfirm p sel = Pictures
  [ p !! 88
  , Translate 50 350 $ Scale 1 1 $ p !! 89
  , Translate (-400) 350 $ Scale 1.2 1.2 $ Color black $ drawWord p "Deseja sair do jogo?"
  , Translate (-300) (-50) $ Scale 1 1 $ (if sel==0 then p !! 21 else p !! 22)
  , Translate (300) (-50) $ Scale 1 1 $ (if sel==1 then p !! 21 else p !! 22)
  , Translate (-460) (-50) $ Scale 1 1 $ drawWord p "Confirmar"
  , Translate (150) (-50) $ Scale 1 1 $ drawWord p "Cancelar"
  ]

-- | Menu principal com seletor expandido (centralizado para 1920x1080)
drawMenu :: [Picture] -> Int -> Picture
drawMenu p sel = Pictures
  [ Scale 1 1 $ p !! 85
  
  -- Título "WORMS"
  , Translate 0 350 $ Scale 1 1 $ p !! 20
  , Translate (-140) 370 $ Scale 1.6 1.6 $ drawWord p "worms"
  
  -- Subtítulo
  , Translate (-210) 290 $ Scale 0.5 0.5 $ drawWord p "Escolha o modo de jogo"
  
  -- Bot Simulation (esquerda superior)
  , Translate (-300) 100 $ Scale 1 1 $ (if sel==0 then p !! 21 else p !! 22)
  , Translate (-300- 150) 100 $ Scale 0.6 0.6 $ Color (if sel==0 then red else black) $ drawWord p "Bot Simulation"
  
  -- Player vs Player (direita superior)
  , Translate 300 100 $ Scale 1 1 $ (if sel==1 then p !! 21 else p !! 22)
  , Translate (300- 160) 100 $ Scale 0.6 0.6 $ Color (if sel==1 then red else black) $ drawWord p "Player vs Player"
  
  -- MAP Creator Tool (esquerda inferior)
  , Translate (-300) (-50) $ Scale 1 1 $ (if sel==2 then p !! 21 else p !! 22)
  , Translate (-300- 160) (-50) $ Scale 0.6 0.6 $ Color (if sel==2 then red else black) $ drawWord p "MAP Creator Tool"
  
  -- Help (direita inferior)
  , Translate 300 (-50) $ Scale 1 1 $ (if sel==3 then p !! 21 else p !! 22)
  , Translate (300 - 50) (-50) $ Scale 0.6 0.6 $ Color (if sel==3 then red else black) $ drawWord p "Help"
  
  -- Quit (centro baixo)
  , Translate 0 (-200) $ Scale 1 1 $ (if sel==4 then p !! 21 else p !! 22)
  , Translate (- 50) (-200) $ Scale 0.6 0.6 $ Color (if sel==4 then red else black) $ drawWord p "Quit"
  ]

getDescription :: Int -> String
getDescription 0 = "Assista bots jogarem automaticamente"
getDescription 1 = "Modo local para dois jogadores"
getDescription 2 = "Crie os seus mapas"
getDescription 3 = "Veja os controles e instruções"
getDescription 4 = "Sair do jogo"
getDescription _ = ""

drawHelp :: [Picture] -> Int -> Picture
drawHelp p pagina = Pictures
  [ 
    Translate (-800) 450 $ Scale 2 2 $ Color black $ drawWord p (titulosPaginas !! pagina)
  

  , conteudoPagina pagina
  

  , Translate (-50) (-450) $ Scale 0.6 0.6 $ Color (greyN 0.5) $ drawWord p ("Pagina " ++ show (pagina + 1) ++ " de " ++ show totalPaginas)
  

  , if pagina > 0 
      then Translate (-320) (-450) $ Scale 0.6 0.6 $ Color (makeColor 0.2 0.4 0.8 1.0) $ drawWord p "< Anterior"
      else Blank
  
  , if pagina < totalPaginas - 1
      then Translate (300) (-450) $ Scale 0.6 0.6 $ Color (makeColor 0.2 0.4 0.8 1.0) $ drawWord p "Proximo >"
      else Blank
  
  , Translate (-800) (-500) $ Scale 0.6 0.6 $ Color (greyN 0.5) $ drawWord p "Use Setas Esq/Dir para navegar | ESC para voltar ao menu"
  ]
  where
    totalPaginas = length titulosPaginas
    
    titulosPaginas = 
      [ "MENU PRINCIPAL"
      , "LEVEL SELECTOR"
      , "MOVIMENTACAO PVP"
      , "CONTROLES PVP"
      , "MAP CREATOR 1"
      , "MAP CREATOR 2"
      ]
    
    conteudoPagina 0 = Pictures
      [ Translate (-800) 300 $ Scale 1.5 1.5 $ Color (makeColor 0.2 0.4 0.8 1.0) $ drawWord p "MENU PRINCIPAL"
      , Translate (-800) 200 $ Scale 1 1 $ Color black $ drawWord p "Setas: Navegar entre opcoes"
      , Translate (-800) 140 $ Scale 1 1 $ Color black $ drawWord p "Enter: Selecionar opcao"
      , Translate (-800) 80 $ Scale 1 1 $ Color black $ drawWord p "ESC: Voltar/Sair"
      , Translate (-800) (-50) $ Scale 1.5 1.5 $ Color (greyN 0.4) $ drawWord p "Opcoes disponiveis:"
      , Translate (-800) (-100) $ Scale 1 1 $ Color black $ drawWord p "- Bot Simulation"
      , Translate (-800) (-140) $ Scale 1 1 $ Color black $ drawWord p "- Player vs Player"
      , Translate (-800) (-180) $ Scale 1 1 $ Color black $ drawWord p "- MAP Creator Tool"
      , Translate (-800) (-220) $ Scale 1 1 $ Color black $ drawWord p "- Help"
      , Translate (-800) (-260) $ Scale 1 1 $ Color black $ drawWord p "- Quit"
      ]
    
    conteudoPagina 1 = Pictures 
      [ Translate (-800) 300 $ Scale 1.5 1.5 $ Color (makeColor 0.2 0.4 0.8 1.0) $ drawWord p "LEVEL SELECTOR"
      , Translate (-800) 200 $ Scale 1 1 $ Color black $ drawWord p "I: Importa mapa salvo"
      , Translate (-800) 140 $ Scale 1 1 $ Color black $ drawWord p "Setas Cima/Baixo: Navegar niveis"
      , Translate (-800) 80 $ Scale 1 1 $ Color black $ drawWord p "Enter: Jogar nivel selecionado"
      , Translate (-800) 20 $ Scale 1 1 $ Color black $ drawWord p "ESC: Voltar ao menu"
      ]
    
    conteudoPagina 2 = Pictures 
      [ Translate (-800) 300 $ Scale 1.5 1.5 $ Color (makeColor 0.2 0.4 0.8 1.0) $ drawWord p "MODO PVP - MOVIMENTACAO"
      , Translate (-800) 200 $ Scale 1 1 $ Color (greyN 0.4) $ drawWord p "Movimento Base:"
      , Translate (-800) 150 $ Scale 1 1 $ Color black $ drawWord p "WASD ou Setas: Norte/Sul/Este/Oeste"
      , Translate (-800) 70 $ Scale 1 1 $ Color (greyN 0.4) $ drawWord p "Movimentos Diagonais:"
      , Translate (-800) 20 $ Scale 1 1 $ Color black $ drawWord p "Q: Noroeste"
      , Translate (-800) (-30) $ Scale 1 1 $ Color black $ drawWord p "E: Nordeste"
      , Translate (-800) (-80) $ Scale 1 1 $ Color black $ drawWord p "Z: Sudoeste"
      , Translate (-800) (-130) $ Scale 1 1 $ Color black $ drawWord p "C: Sudeste"
      ]
    
    conteudoPagina 3 = Pictures 
      [ Translate (-800) 300 $ Scale 1.5 1.5 $ Color (makeColor 0.2 0.4 0.8 1.0) $ drawWord p "MODO PVP - CONTROLES"
      , Translate (-800) 200 $ Scale 1 1 $ Color black $ drawWord p "1: Trocar minhoca"
      , Translate (-800) 160 $ Scale 1 1 $ Color (greyN 0.4) $ drawWord p "  (Cicla para a proxima minhoca viva)"
      , Translate (-800) 90 $ Scale 1 1 $ Color black $ drawWord p "2: Trocar arma"
      , Translate (-800) 50 $ Scale 1 1 $ Color (greyN 0.4) $ drawWord p "  Ciclo: Jetpack -> Escavadora -> Bazuca"
      , Translate (-800) 20 $ Scale 1 1 $ Color (greyN 0.4) $ drawWord p "         -> Mina -> Dinamite -> Nenhuma"
      , Translate (-800) (-50) $ Scale 1 1 $ Color black $ drawWord p "ESC: Voltar ao menu"
      ]
    
    conteudoPagina 4 = Pictures 
      [ Translate (-800) 300 $ Scale 1.5 1.5 $ Color (makeColor 0.2 0.4 0.8 1.0) $ drawWord p "MAP CREATOR - GERAL"
      , Translate (-800) 200 $ Scale 1 1 $ Color (greyN 0.4) $ drawWord p "Navegacao:"
      , Translate (-800) 160 $ Scale 1 1 $ Color black $ drawWord p "1: Alternar modo"
      , Translate (-800) 130 $ Scale 1 1 $ Color (greyN 0.4) $ drawWord p "  (Blocos -> Objetos -> Minhocas)"
      , Translate (-800) 80 $ Scale 1 1 $ Color black $ drawWord p "Setas Cima/Baixo: Navegar opcoes"
      , Translate (-800) 40 $ Scale 1 1 $ Color black $ drawWord p "Setas Esq/Dir: Navegar subopcoes"
      , Translate (-800) 0 $ Scale 1 1 $ Color black $ drawWord p "ESC: Voltar ao menu"
      , Translate (-800) (-70) $ Scale 1 1 $ Color (greyN 0.4) $ drawWord p "Mouse:"
      , Translate (-800) (-110) $ Scale 1 1 $ Color black $ drawWord p "Clique Esquerdo: Adicionar elemento"
      , Translate (-800) (-150) $ Scale 1 1 $ Color black $ drawWord p "Clique Direito: Remover elemento"
      , Translate (-800) (-220) $ Scale 0.7 0.7 $ Color (greyN 0.4) $ drawWord p "(Remove apenas do modo selecionado)"
      ]
    
    conteudoPagina 5 = Pictures 
      [ Translate (-800) 350 $ Scale 1.5 1.5 $ Color (makeColor 0.2 0.4 0.8 1.0) $ drawWord p "MAP CREATOR - EDICAO"
      , Translate (-800) 270 $ Scale 1 1 $ Color (greyN 0.4) $ drawWord p "Edicao do Mapa:"
      , Translate (-800) 230 $ Scale 1 1 $ Color black $ drawWord p "K: Adicionar linha abaixo"
      , Translate (-800) 195 $ Scale 1 1 $ Color black $ drawWord p "L: Adicionar coluna a direita"
      , Translate (-800) 160 $ Scale 1 1 $ Color black $ drawWord p "N: Remover ultima linha"
      , Translate (-800) 125 $ Scale 1 1 $ Color black $ drawWord p "M: Remover ultima coluna"
      , Translate (-800) 60 $ Scale 1 1 $ Color (greyN 0.4) $ drawWord p "Edicao de Minhocas:"
      , Translate (-800) 20 $ Scale 1 1 $ Color black $ drawWord p "Enter: Ativar/desativar modo edicao"
      , Translate (-800) (-15) $ Scale 1 1 $ Color black $ drawWord p "0-9: Digitar valores"
      , Translate (-800) (-50) $ Scale 1 1 $ Color black $ drawWord p "Backspace/Delete: Apagar digito"
      , Translate (-800) (-85) $ Scale 1 1 $ Color black $ drawWord p "Seta Esq/Dir: Navegar atributos"
      , Translate (-800) (-150) $ Scale 1 1 $ Color (greyN 0.4) $ drawWord p "Salvar/Carregar:"
      , Translate (-800) (-185) $ Scale 1 1 $ Color black $ drawWord p "E: Exportar estado"
      , Translate (-800) (-220) $ Scale 0.7 0.7 $ Color (greyN 0.4) $ drawWord p "  (Salva em 'estado.txt')"
      ]
    
    conteudoPagina _ = Blank

-- | Desenha o selector de níveis como uma lista vertical
drawLvlSelector :: [Picture] -> Int -> [EstadoDLC] -> Picture
drawLvlSelector p selected estadosImportados = Pictures
 ([p !! 88] ++ (zipWith drawNivel [0..] estadosImportados))

  where
    drawNivel :: Int -> EstadoDLC -> Picture
    drawNivel idx estado = Translate 0 y $ Pictures
      [ Scale 1 1 $ (if idx == selected then p !! 89 else p !! 90)
      , Translate (x - largura/4) 0 $ Scale 0.7 0.7 $ drawWord p ("Level " ++ show (idx + 1))
      , Translate (x + largura/4 - 100) 0 $ Scale 0.6 0.6 $ Color black $ drawWord p infoMapa
      , Translate (x - 20) (-140) $ Scale 0.9 0.9 $ p !! 91
      ]
      where
        mapa = mapaEstadoDLC estado
        infoMapa = show (length mapa) ++ "x" ++ show (if null mapa then 0 else length (head mapa))
        largura = 1000
        altura  = 50
        espaco  = 90
        x = 20

        y = 300 - fromIntegral idx * (altura + espaco)
  
  
drawGameOver :: [Picture] -> Team -> Picture
drawGameOver p equipa =
    Pictures
        [ bg
        , Pictures
            [Translate (-200) 100 $ Scale 0.8 0.8 $ Color (equipaCor equipa) $
                drawWord p (equipaStr equipa ++ " Ganha!")
            ]
        , Pictures
            [ Translate (70) (-50) $ Color white $ rectangleSolid 580 20
            , Translate (-200) (-50) $ Scale 0.4 0.4 $ Color (greyN 0.7) $
                drawWord p "Pressiona ESC para retornar ao menu"
            ]
        ]
  where
    bg = case equipa of
      Red -> Pictures [Translate 0 0 $ Scale 1 1 $ p !! 115, Translate (125) 100 $ Color white $ rectangleSolid 720 60]
      Blue -> Pictures [Translate 0 0 $ Scale 1 1 $ p !! 116, Translate (70) 100 $ Color white $ rectangleSolid 580 60]

    equipaStr Red = "Equipa Vermelha"
    equipaStr Blue = "Equipa Azul"
    equipaCor Red = red
    equipaCor Blue = blue

drawMCT :: [Picture] -> EstadoDLC -> Int -> Int -> Int -> Int -> Bool -> Maybe Int -> MinhocaDLC -> Picture
drawMCT p e blocoSelecionado mode secSel thirdSel editMode _ (MinhocaDLC _ _ jet esc baz mina dina flame burn equipa _) = Pictures
  [ Translate (-440) 330 $ Scale 0.5 0.5 $ Color black $ drawWord p "Bem vindo ao criador de mapas", sidebar, world]
  where
    mapa = mapaEstadoDLC e
    objs = objetosEstadoDLC e
    ms = minhocasEstadoDLC e

    infoMapa = show (length mapa) ++ "x" ++ show (if null mapa then 0 else length (head mapa))

    blocos = [(0, "Terra", Scale 0.66 0.66 $ p !! 0), (1, "Agua", p !! 1), (2, "Pedra", p !! 2), (3, "Ar", p !! 7), (4, "Lava", p !! 11) ]
    
    staticObjects = [(0, "Barril", p !! 5), (1, "Health Pack", p !! 12)]
    ammoPacks = [(2 :: Int, "Jetpack", p !! 15), (2, "Escavadora", p !! 16), (2, "Bazuca", p !! 17), (2, "Mina", p !! 18), (2, "Dinamite", p !! 19)]
    disparos = [(3 :: Int, "Bazuca", p !! 6), (3, "Mina", p !! 9), (3, "Dinamite", p !! 8), (3, "FireBall", p !! 69)]
    
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
      [ Translate (-750) 80 $ p !! 94
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
    
    larguraMapa = fromIntegral cols * cellSize
    alturaMapa  = fromIntegral linha * cellSize
    
    sidebarWidth = 1000
    maxWorldWidth  = janelaLargura - sidebarWidth
    maxWorldHeight = janelaAltura - 400
    
    scaleX = if larguraMapa > 0 then maxWorldWidth / larguraMapa else 1
    scaleY = if alturaMapa > 0 then maxWorldHeight / alturaMapa else 1
    
    scaleFactor = min (min scaleX scaleY) 2.0

    editColor = if editMode == False then makeColor 0.3 0.6 1.0 0.3 else red

    world =
      Translate 50 0 $
        Scale scaleFactor scaleFactor $
          Pictures
            [ Color (greyN 0.3) $ rectangleWire larguraMapa alturaMapa
            , drawMapaDLC p mapa
            , drawObjetosDLC p objs mapa
            , drawMinhocasStatic p ms mapa
            ]

cellSize :: Float
cellSize = 32

drawGame :: [Picture] -> Estado -> Maybe NumMinhoca -> Maybe Jogada -> Picture
drawGame p est numMinhoca jogada = Pictures [p !! 88, sidebar, world]
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
    
    larguraMapa = fromIntegral cols * cellSize
    alturaMapa  = fromIntegral linha * cellSize
    
    sidebarWidth = 200
    maxWorldWidth  = janelaLargura - sidebarWidth
    maxWorldHeight = janelaAltura - 400
    
    scaleX = if larguraMapa > 0 then maxWorldWidth / larguraMapa else 1
    scaleY = if alturaMapa > 0 then maxWorldHeight / alturaMapa else 1
    
    scaleFactor = min (min scaleX scaleY) 2.0

   
    world =
      Translate (sidebarWidth / 2) 0 $
        Scale scaleFactor scaleFactor $
          Pictures
            [ drawMapa p mapa
            , drawObjetos p objs mapa
            , drawMinhocas p ms mapa numMinhoca jogada
            ]

drawPvPGame :: [Picture] -> EstadoDLC -> JogadaDLC -> Picture
drawPvPGame p est jogada =
  Pictures [p !! 88, p!!97, sidebar, world]
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

    selectedSprite :: Picture
    selectedSprite =
      let idx = minhocaSelecionada est
      in if idx >= 0 && idx < length ms
         then case equipaMinhoca (ms !! idx) of
                Just Blue -> p !! 95
                Just Red -> p !! 96
                _ -> p !! 95
         else p !! 95

    sidebar =
      Pictures
        ( [ Translate (-750) 0 $ Scale 1.1 1.1 $ selectedSprite
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


    weaponOffset :: Float
    weaponOffset = 120

    minhocasSidebar :: [Picture]
    minhocasSidebar =
      let minhocaAtualIndex = minhocaSelecionada est
      in if minhocaAtualIndex >= 0 && minhocaAtualIndex < length ms
         then [drawMinhocaBar minhocaAtualIndex (ms !! minhocaAtualIndex)]
         else []

    drawMinhocaBar :: Int -> MinhocaDLC -> Picture
    drawMinhocaBar i m =
      Pictures
          [  -- Nome / índice
            Translate (-800) y $
            Scale 0.6 0.6 $
            Color (if armaSelecionada est == Nothing then dark red else black) $
                  drawWord p ("Player " ++ show i)
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
          , Translate (-930) (y - weaponOffset) $ Scale 1.5 1.5 $ p !! 113
          , Translate (-840) (y - weaponOffset) $ Scale 1.5 1.5 $ p !! 114
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
            Color (if armaSelecionada est == Just JetpackDLC then dark red else black) $
                  drawWord p (show (jetpackMinhocaDLC m) ++ " ")
          , Translate (-848) (y - weaponOffset - 60) $
            Scale 0.6 0.6 $
            Color (if armaSelecionada est == Just EscavadoraDLC then dark red else black) $
                  drawWord p (show (escavadoraMinhocaDLC m) ++ " ")
          , Translate (-758) (y - weaponOffset - 60) $
            Scale 0.6 0.6 $
            Color (if armaSelecionada est == Just BazucaDLC then dark red else black) $
                  drawWord p (show (bazucaMinhocaDLC m) ++ " ")
          , Translate (-668) (y - weaponOffset - 60) $
            Scale 0.6 0.6 $
            Color (if armaSelecionada est == Just MinaDLC then dark red else black) $
                  drawWord p (show (minaMinhocaDLC m) ++ " ")
          , Translate (-578) (y - weaponOffset - 60) $
            Scale 0.6 0.6 $
            Color (if armaSelecionada est == Just DinamiteDLC then dark red else black) $
                  drawWord p (show (dinamiteMinhocaDLC m) ++ " ")
          ]
        where
          y = 55

    linha = length mapa
    cols = if null mapa then 0 else length (head mapa)
    
    larguraMapa = fromIntegral cols * cellSize
    alturaMapa  = fromIntegral linha * cellSize
    
    sidebarWidth = 1000
    maxWorldWidth  = janelaLargura - sidebarWidth
    maxWorldHeight = janelaAltura - 400
    
    scaleX = if larguraMapa > 0 then maxWorldWidth / larguraMapa else 1
    scaleY = if alturaMapa > 0 then maxWorldHeight / alturaMapa else 1
    
    scaleFactor = min (min scaleX scaleY) 2.0


    world =
      Translate 100 0 $
        Scale scaleFactor scaleFactor $
          Pictures
            [ drawMapaDLC p mapa
            , drawObjetosDLC p objs mapa
            , drawMinhocasDLC p ms mapa (Just 0) jogada est
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
    _ -> Blank









bazucaDir :: [Picture] -> Direcao -> Picture
bazucaDir p dir = case dir of
    Este -> p !! 6
    Oeste -> Rotate 180 (p !! 6)
    Norte -> Rotate 270 (p !! 6)
    Sul -> Rotate 90 (p !! 6)
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



getSpriteParaAcao :: Minhoca -> Maybe Jogada -> [Picture] -> Bool -> Mapa -> Posicao -> Picture
getSpriteParaAcao _ Nothing p _ _ _ = p !! 3  -- Idle

getSpriteParaAcao minhoca (Just (Labs2025.Move dir)) p isActiveMinhoca mapa _

  | eMinhocaViva minhoca =
      let pos = case posicaoMinhoca minhoca of Just a -> a
          estaNoChao = Tarefa2.estaNoSolo pos mapa
          direcaoHorizontal = getXWay dir
      in if isActiveMinhoca then
        case direcaoHorizontal of
            Este -> if estaNoChao then p !! 99 else p !! 98
            _ ->  if estaNoChao then p !! 3 else p !! 13
        else if estaNoChao then p !! 3 else p !! 13


getSpriteParaAcao minhoca (Just (Labs2025.Dispara arma direcaoDisparo)) p isActiveMinhoca mapa _
  = let
        pos = case posicaoMinhoca minhoca of Just a -> a
        estaNoChao = Tarefa2.estaNoSolo pos mapa
        direcaoHorizontal = getXWay direcaoDisparo
    in 
      if isActiveMinhoca 
        then
          case direcaoHorizontal of
          
          Este ->

            case arma of
              Bazuca -> p !! 99
              Jetpack ->  p !! 99
              Escavadora -> p !! 99
              Dinamite -> p !! 99
              Mina -> p !! 99

          Oeste -> 
          
            case arma of
              Bazuca -> p !! 3
              Jetpack ->  p !! 3
              Escavadora -> p !! 3
              Dinamite -> p !! 3
              Mina -> p !! 3
      else if estaNoChao then p !! 3 else p !! 13  






drawMinhocasStatic :: [Picture] -> [MinhocaDLC] -> MapaDLC -> Picture
drawMinhocasStatic p minhocas mapa = Pictures $ map drawM minhocas
  where
    drawM m = case posicaoMinhocaDLC m of
      Nothing -> Blank
      Just pos ->
        let (x, y) = converteMapaDLC mapa pos
            sprite = case vidaMinhocaDLC m of
              MortaDLC -> p !! 4
              VivaDLC _ -> case equipaMinhoca m of
                Just Blue -> p !! 78
                Just Red -> p !! 79
                _ -> p !! 3

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

getSpriteParaAcaoDLC :: MinhocaDLC -> JogadaDLC -> [Picture] -> EstadoDLC -> Picture
getSpriteParaAcaoDLC minhoca (DataDLC.Move _) p e
  | burningCounter minhoca > 0 =
      case ultimaDirecaoHorizontal minhoca of
        Oeste -> p !! 84  
        Este  -> p !! 100 
        
  | eMinhocaVivaDLC minhoca =
      let pos = case posicaoMinhocaDLC minhoca of Just a -> a
          estaNoChao = EfetuaJogada.estaNoSolo pos (mapaEstadoDLC e) (minhocasEstadoDLC e)
          direcaoHorizontal = ultimaDirecaoHorizontal minhoca
      in case equipaMinhoca minhoca of

        Just Red ->
          case direcaoHorizontal of
            Este -> 
              if estaNoChao then p !! 104 else p !! 103
            _ ->  
              if estaNoChao then p !! 78 else p !! 77

        Just Blue ->
          case direcaoHorizontal of
            Este ->
              if estaNoChao then p !! 110 else p !! 109
            _ ->
              if estaNoChao then p !! 79 else p !! 76

        _ ->
          case direcaoHorizontal of
            Este -> p !! 98
            _ -> p !! 3

getSpriteParaAcaoDLC minhoca (DataDLC.Dispara arma _) p e
  | burningCounter minhoca > 0 = 
      case ultimaDirecaoHorizontal minhoca of
        Oeste -> p !! 84
        Este  -> p !! 100
        
  | eMinhocaVivaDLC minhoca =
      let pos = case posicaoMinhocaDLC minhoca of Just a -> a
          estaNoChao = EfetuaJogada.estaNoSolo pos (mapaEstadoDLC e) (minhocasEstadoDLC e)
          direcaoHorizontal = ultimaDirecaoHorizontal minhoca
      in case equipaMinhoca minhoca of

        Just Red -> case arma of
          JetpackDLC     -> 
            case direcaoHorizontal of
              Este -> p !! 109
              _ -> p !! 86
          EscavadoraDLC  -> 
            case direcaoHorizontal of
              Este -> p !! 107
              _ -> p !! 83
          BazucaDLC      -> 
            case direcaoHorizontal of
              Este -> p !! 105
              _ -> p !! 72
          _ ->
            case direcaoHorizontal of
              Este -> if estaNoChao then p !! 101 else p !! 102
              _ -> if estaNoChao then p !! 78 else p !! 77

        Just Blue -> case arma of
          JetpackDLC     -> 
            case direcaoHorizontal of
              Este -> p !! 110
              _ -> p !! 87
          EscavadoraDLC  -> 
            case direcaoHorizontal of
              Este -> p !! 108
              _ -> p !! 82
          BazucaDLC      -> 
            case direcaoHorizontal of
              Este -> p !! 106
              _ -> p !! 73
          _ ->
            case direcaoHorizontal of
              Este -> if estaNoChao then p !! 103 else p !! 104
              _ -> if estaNoChao then p !! 79 else p !! 76

        _ ->
          case direcaoHorizontal of
            Este -> p !! 98
            _ -> p !! 3

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
-- | Desenha as minhocas com sprites diferentes baseado na última jogada
drawMinhocasDLC :: [Picture] -> [MinhocaDLC] -> MapaDLC -> Maybe NumMinhoca -> JogadaDLC -> EstadoDLC -> Picture
drawMinhocasDLC p ms mapa _ jogada e = Pictures $ map drawM (zip [0..] ms)
  where
    drawM (i,m) = case posicaoMinhocaDLC m of
      Nothing -> Blank
      Just s -> Translate x y $ Pictures
        [ sprite
        , if i == minhocaSelecionada e
            then if eMinhocaVivaDLC m
              then case ultimaDirecaoHorizontal m of
                Oeste -> if EfetuaJogada.estaNoSolo s mapa ms then Scale 1 1 $ p !! 92 else Scale 1 1 $ p !! 93
                Este -> if EfetuaJogada.estaNoSolo s mapa ms then Scale 1 1 $ p !! 101 else Scale 1 1 $ p !! 102
              else Blank
            else Blank
        ]
        where
          (x,y) = converteMapaDLC mapa s
          sprite = if vidaMinhocaDLC m == MortaDLC
            then p !! 4  -- Morto
            else getSpriteParaAcaoDLC m jogada p e

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

