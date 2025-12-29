module Main where

import Graphics.Gloss

import Desenhar
import Eventos
import Worms
import Tempo

import Graphics.Gloss.Juicy
import Graphics.Gloss.Interface.IO.Game



janela :: Display
janela = InWindow "Worms" (1920, 1080) (0,0)

fundo :: Color
fundo = white


fr :: Int
fr = 2

path :: String
path = "app/sprites/"

main :: IO ()
main = do

  writeFile "estado.txt" "" -- * reset ao estado salvo em cada jogo novo

  putStrLn "Iniciando Worms - demo"

  Just grass  <- loadJuicy (path ++"Grama.png")
  Just water  <- loadJuicy (path ++"water.png")
  Just stone  <- loadJuicy (path ++"Pedra.png")
  Just sky    <- loadJuicy (path ++"sky.png")
  Just dirt  <- loadJuicy (path ++"Terra.png")
  Just lava <- loadJuicy (path ++ "lava.png")


  Just  morto  <- loadJuicy (path ++"Dead.png")
  Just  barril <- loadJuicy (path ++"Barril.png")
  Just  bazuca <- loadJuicy (path ++"Bazuca.png")
  Just  dinamite <- loadJuicy (path ++"Dinamite.png")
  Just  mina <- loadJuicy (path ++"Mina.png")
  Just healthPack <- loadJuicy (path ++ "health_pack.png")
  
  Just ammoJetpack <- loadJuicy (path ++ "ammo_box_jetpack.png")
  Just ammoEscavadora <- loadJuicy (path ++ "ammo_box_escavadora.png")
  Just ammoBazuca <- loadJuicy (path ++ "ammo_box_bazuca.png")
  Just ammoMina <- loadJuicy (path ++ "ammo_box_mina.png")
  Just ammoDinamite <- loadJuicy (path ++ "ammo_box_dinamite.png")
  Just botao <- loadJuicy (path ++ "BotaoMenuapagado.png")
  Just botaoOn <- loadJuicy (path ++ "BotaoMenuSelecionado.png")
  Just botaoOff <- loadJuicy (path ++ "BotaoMenuDescelecionado.png")
  Just botaoOn2 <- loadJuicy (path ++ "BotaoMenuSelecionado2.png")
  Just botaoOff2 <- loadJuicy (path ++ "BotaoMenuDescelecionado2.png")
  Just jetpack <- loadJuicy (path ++ "Jetpack.png")
  Just escavadora <- loadJuicy (path ++ "Escavadora.png")
  
  Just background <- loadJuicy (path ++ "Background.png")
  Just backgroundBorrado <- loadJuicy (path ++ "BackgroundBorrado.png")
  Just sidebar <- loadJuicy (path ++ "Sidebar.png")
  Just sidebarAzul <- loadJuicy (path ++ "SidebarPlayerAzul.png")
  Just sidebarVermelho <- loadJuicy (path ++ "SidebarPlayerVermelha.png")
  Just redWins <- loadJuicy (path ++ "RedWins.png")
  Just blueWins <- loadJuicy (path ++ "BlueWins.png")

  -- Pato normal
  Just patoPulando <- loadJuicy (path ++ "PatoPulando.png")
  Just pato   <- loadJuicy (path ++"PatoParado.png")
  Just patoFogo <- loadJuicy (path ++ "PatoFogo.png")
  Just patoOutline   <- loadJuicy (path ++"PatoOutline.png")
  Just patoOutlinePulando   <- loadJuicy (path ++"PatoOutlinePulando.png")
  
  -- Pato normal Right
  Just patoPulandoR <- loadJuicy (path ++ "PatoPulandoRight.png")
  Just patoR   <- loadJuicy (path ++"PatoParadoRight.png")
  Just patoFogoR <- loadJuicy (path ++ "PatoFogoRight.png")
  Just patoFogoOutline <- loadJuicy (path ++ "PatoFogoOutline.png")
  Just patoFogoOutlineR <- loadJuicy (path ++ "PatoFogoOutlineRight.png")

  Just patoOutlineR   <- loadJuicy (path ++"PatoOutlineRight.png")
  Just patoOutlinePulandoR   <- loadJuicy (path ++"PatoOutlinePulandoRight.png")
  
  -- Pato vermelho
  Just patoVermelhoPulando <- loadJuicy (path ++ "PatoVermelhoPulando.png")
  Just patoVermelhoParado <- loadJuicy (path ++ "PatoVermelhoParado.png")
  Just patoBazucaVermelho <- loadJuicy (path ++ "PatoBazucaVermelho.png")
  Just patoEscavadoraVermelho <- loadJuicy (path ++ "PatoEscavadoraVermelho.png")
  Just patoVermelhoJetpack <- loadJuicy (path ++ "PatoVermelhoJetpack.png")
  
  -- Pato vermelho Right
  Just patoVermelhoPulandoR <- loadJuicy (path ++ "PatoVermelhoPulandoRight.png")
  Just patoVermelhoParadoR <- loadJuicy (path ++ "PatoVermelhoParadoRight.png")
  Just patoBazucaVermelhoR <- loadJuicy (path ++ "PatoBazucaVermelhoRight.png")
  Just patoEscavadoraVermelhoR <- loadJuicy (path ++ "PatoEscavadoraVermelhoRight.png")
  Just patoVermelhoJetpackR <- loadJuicy (path ++ "PatoVermelhoJetpackRight.png")
  
  -- Pato Azul
  Just patoBazucaAzul <- loadJuicy (path ++ "PatoBazucaAzul.png")
  Just patoAzulPulando <- loadJuicy (path ++ "PatoAzulPulando.png")
  Just patoAzulParado <- loadJuicy (path ++ "PatoAzulParado.png")
  Just patoEscavadoraAzul <- loadJuicy (path ++ "PatoEscavadoraAzul.png")
  Just patoAzulJetpack <- loadJuicy (path ++ "PatoAzulJetpack.png")
  
  -- Pato Azul Right
  Just patoBazucaAzulR <- loadJuicy (path ++ "PatoBazucaAzulRight.png")
  Just patoAzulPulandoR <- loadJuicy (path ++ "PatoAzulPulandoRight.png")
  Just patoAzulParadoR <- loadJuicy (path ++ "PatoAzulParadoRight.png")
  Just patoEscavadoraAzulR <- loadJuicy (path ++ "PatoEscavadoraAzulRight.png")
  Just patoAzulJetpackR <- loadJuicy (path ++ "PatoAzulJetpackRight.png")

  Just mapSheet <- loadJuicy (path ++ "MapSheet.png")
  -- * Fontes

  Just a <- loadJuicy (path ++ "fontes/A.png")
  Just b <- loadJuicy (path ++ "fontes/B.png")
  Just c <- loadJuicy (path ++ "fontes/C.png")
  Just d <- loadJuicy (path ++ "fontes/D.png")
  Just e <- loadJuicy (path ++ "fontes/E.png")
  Just f <- loadJuicy (path ++ "fontes/F.png")
  Just g <- loadJuicy (path ++ "fontes/G.png")
  Just h <- loadJuicy (path ++ "fontes/H.png")
  Just i <- loadJuicy (path ++ "fontes/I.png")
  Just j <- loadJuicy (path ++ "fontes/J.png")
  Just k <- loadJuicy (path ++ "fontes/K.png")
  Just l <- loadJuicy (path ++ "fontes/L.png")
  Just m <- loadJuicy (path ++ "fontes/M.png")
  Just n <- loadJuicy (path ++ "fontes/N.png")
  Just o <- loadJuicy (path ++ "fontes/O.png")
  Just p <- loadJuicy (path ++ "fontes/P.png")
  Just q <- loadJuicy (path ++ "fontes/Q.png")
  Just r <- loadJuicy (path ++ "fontes/R.png")
  Just s <- loadJuicy (path ++ "fontes/S.png")
  Just t <- loadJuicy (path ++ "fontes/T.png")
  Just u <- loadJuicy (path ++ "fontes/U.png")
  Just v <- loadJuicy (path ++ "fontes/V.png")
  Just w <- loadJuicy (path ++ "fontes/W.png")
  Just x <- loadJuicy (path ++ "fontes/X.png")
  Just y <- loadJuicy (path ++ "fontes/Y.png")
  Just z <- loadJuicy (path ++ "fontes/Z.png")
  Just _0 <- loadJuicy (path ++ "fontes/0.png")
  Just _1 <- loadJuicy (path ++ "fontes/1.png")
  Just _2 <- loadJuicy (path ++ "fontes/2.png")
  Just _3 <- loadJuicy (path ++ "fontes/3.png")
  Just _4 <- loadJuicy (path ++ "fontes/4.png")
  Just _5 <- loadJuicy (path ++ "fontes/5.png")
  Just _6 <- loadJuicy (path ++ "fontes/6.png")
  Just _7 <- loadJuicy (path ++ "fontes/7.png")
  Just _8 <- loadJuicy (path ++ "fontes/8.png")
  Just _9 <- loadJuicy (path ++ "fontes/9.png")
  Just dash <- loadJuicy (path ++ "fontes/dash.png")
  Just pl <- loadJuicy (path ++ "fontes/pl.png")
  Just pr <- loadJuicy (path ++ "fontes/pr.png")
  Just slash <- loadJuicy (path ++ "fontes/slash.png")
  Just dott_points <- loadJuicy (path ++ "fontes/dott_points.png")
  Just comma <- loadJuicy (path ++ "fontes/comma.png")
  



  Just space <- loadJuicy (path ++ "fontes/blank.png")
  Just exclamation <- loadJuicy (path ++ "fontes/exclamation.png")
  Just interrogation <- loadJuicy (path ++ "fontes/interrogation.png")
  Just arrombado <- loadJuicy (path ++ "fontes/arrombado.png")
  Just leftArrow <- loadJuicy (path ++ "fontes/leftarrow.png")
  Just rightArrow <- loadJuicy (path ++ "fontes/rightarrow.png")

  Just fireball <- loadJuicy (path ++ "fireball.png")

  Just lvlAddSign <- loadJuicy (path ++ "AdicioneLevelSign.png")
  
  Just blank_sprite <- loadJuicy (path ++ "WormArcher.png") -- * Usado apenas para encher espacos para evitar conflitos apos major change
  Just selectedOutline <- loadJuicy (path ++ "border.png") -- * Usado apenas para encher espacos para evitar conflitos apos major change


  let tiles = [grass, water, stone, pato, morto, barril, bazuca, sky, dinamite, mina, dirt, lava, healthPack, patoPulando, blank_sprite, ammoJetpack, ammoEscavadora, ammoBazuca, ammoMina, ammoDinamite, botao, botaoOn, botaoOff, a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, space, _0, _1, _2, _3, _4, _5, _6, _7, _8, _9, dash, pl, pr, slash, dott_points, comma, exclamation, interrogation, arrombado, fireball, leftArrow, rightArrow, patoBazucaVermelho, patoBazucaAzul, blank_sprite, blank_sprite, patoAzulPulando, patoVermelhoPulando, patoVermelhoParado, patoAzulParado, blank_sprite, blank_sprite, patoEscavadoraAzul, patoEscavadoraVermelho, patoFogo, background, patoVermelhoJetpack, patoAzulJetpack, backgroundBorrado, botaoOn2, botaoOff2, lvlAddSign, patoOutline, patoOutlinePulando, sidebar, sidebarAzul, sidebarVermelho, mapSheet, patoPulandoR, patoR, patoFogoR, patoOutlineR, patoOutlinePulandoR, patoVermelhoPulandoR, patoVermelhoParadoR, patoBazucaVermelhoR, patoEscavadoraVermelhoR, patoVermelhoJetpackR, patoBazucaAzulR, patoAzulPulandoR, patoAzulParadoR, patoEscavadoraAzulR, patoAzulJetpackR, jetpack, escavadora, redWins, blueWins, selectedOutline, patoFogoOutline, patoFogoOutlineR]

{- Indices dos sprites:
 0: grass, 1: water, 2: stone, 3: pato, 4: morto, 5: barril,
 6: bazuca, 7: sky, 8: dinamite, 9: mina, 10: dirt,
 11: lava, 12: healthPack, 13: patoPulando, 14: patoCaindo,
 15-19: ammo packs, 20-22: botoes menu, 23-68: fontes,
 69: fireball, 70-71: setas, 72: patoBazucaVermelho, 73: patoBazucaAzul,
 74: patoAzulCaindo, 75: patoVermelhoCaindo, 76: patoAzulPulando,
 77: patoVermelhoPulando, 78: patoVermelhoParado, 79: patoAzulParado,
 80: PatoMachudadoAzul, 81: PatoMachudadoVermelho,
 82: patoEscavadoraAzul, 83: patoEscavadoraVermelho, 84: patoFogo, 85: background, 
 86: patoVermelhoJetpack, 87: patoAzulJetpack, 88: backgroundBorrado
  89-90: botoes menu 2, 91: lvlAddSign, 92-94: patoOutline, patoOutlinePulando,
  95-97: sidebar, sidebarAzul, sidebarVermelho, 98: mapSheet,
  99-103: pato normal Right, 104-108: pato vermelho Right,
  109-112: pato Azul Right, 113: jetpack, 114: escavadora
  115: redWins, 116: blueWins
-}

  -- let temanatal = [grass, water, stone, pato, morto, barril, bazuca, sky, dinamite, mina, dirt, lava, healthPack, patoPulando, patoCaindo, ammoJetpack, ammoEscavadora, ammoBazuca, ammoMina, ammoDinamite, botao, botaoOn, botaoOff, a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, space, _0, _1, _2, _3, _4, _5, _6, _7, _8, _9, dash, pl, pr, slash, dott_points, comma, exclamation, interrogation]


  playIO janela fundo fr it (desenha tiles) reageEventos reageTempo
  where
    
    it = Menu 0

