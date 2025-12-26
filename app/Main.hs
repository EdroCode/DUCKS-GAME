module Main where

import Graphics.Gloss

import Desenhar
import Eventos
import Worms
import Tempo

import Graphics.Gloss.Juicy
import Graphics.Gloss.Interface.IO.Game


janela :: Display
janela = InWindow "Worms" (1920, 1080) (0, 0)

fundo :: Color
fundo = white


fr :: Int
fr = 2

path :: String
path = "app/sprites/"

main :: IO ()
main = do
  putStrLn "Iniciando Worms - demo"

  Just grass  <- loadJuicy (path ++"Grama.png")
  Just water  <- loadJuicy (path ++"water.png")
  Just stone  <- loadJuicy (path ++"Pedra.png")
  Just sky    <- loadJuicy (path ++"sky.png")
  Just dirt  <- loadJuicy (path ++"Terra.png")
  Just lava <- loadJuicy (path ++ "lava.png")


  Just worm   <- loadJuicy (path ++"PatoParado.png")
  Just  morto  <- loadJuicy (path ++"Dead.png")
  Just  barril <- loadJuicy (path ++"Barril.png")
  Just  bazuca <- loadJuicy (path ++"Bazuca.png")
  Just  dinamite <- loadJuicy (path ++"Dinamite.png")
  Just  mina <- loadJuicy (path ++"Mina.png")
  Just patoPulando <- loadJuicy (path ++ "PatoPulando.png")
  Just patoCaindo <- loadJuicy (path ++ "PatoCaindo.png")
  Just healthPack <- loadJuicy (path ++ "health_pack.png")
  
  Just ammoJetpack <- loadJuicy (path ++ "ammo_box_jetpack.png")
  Just ammoEscavadora <- loadJuicy (path ++ "ammo_box_escavadora.png")
  Just ammoBazuca <- loadJuicy (path ++ "ammo_box_bazuca.png")
  Just ammoMina <- loadJuicy (path ++ "ammo_box_mina.png")
  Just ammoDinamite <- loadJuicy (path ++ "ammo_box_dinamite.png")
  Just botao <- loadJuicy (path ++ "BotaoMenuapagado.png")
  Just botaoOn <- loadJuicy (path ++ "BotaoMenuSelecionado.png")
  Just botaoOff <- loadJuicy (path ++ "BotaoMenuDescelecionado.png")

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
  Just space <- loadJuicy (path ++ "fontes/blank.png")


  let tiles = [grass, water, stone, worm, morto, barril, bazuca, sky, dinamite, mina, dirt, lava, healthPack, patoPulando, patoCaindo, ammoJetpack, ammoEscavadora, ammoBazuca, ammoMina, ammoDinamite, botao, botaoOn, botaoOff, a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, space]

  let temanatal = [grass, water, stone, worm, morto, barril, bazuca, sky, dinamite, mina]

  playIO janela fundo fr it (desenha tiles) reageEventos reageTempo
  where
    
    it = Menu 0

