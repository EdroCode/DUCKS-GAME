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



  let tiles = [grass, water, stone, worm, morto, barril, bazuca, sky, dinamite, mina, dirt, lava, healthPack, patoPulando, patoCaindo, ammoJetpack, ammoEscavadora, ammoBazuca, ammoMina, ammoDinamite]

  let temanatal = [grass, water, stone, worm, morto, barril, bazuca, sky, dinamite, mina]

  playIO janela fundo fr it (desenha tiles) reageEventos reageTempo
  where
    
    it = Menu 0

