module Main where

import Graphics.Gloss

import Desenhar
import Eventos
import Worms
import Tempo

import Graphics.Gloss.Juicy



janela :: Display
janela = InWindow "Worms" (1920, 1080) (0, 0)

fundo :: Color
fundo = white


fr :: Int
fr = 60


main :: IO ()
main = do
  putStrLn "Iniciando Worms - demo"

  Just grass  <- loadJuicy "app/sprites/grass1.png"
  Just water  <- loadJuicy "app/sprites/water.png"
  Just stone  <- loadJuicy "app/sprites/stone1.png"
  Just sky    <- loadJuicy "app/sprites/sky.png"
  worm   <- loadBMP "app/sprites/Worm.bmp"
  morto  <- loadBMP "app/sprites/Dead.bmp"
  barril <- loadBMP "app/sprites/Barril.bmp"
  bazuca <- loadBMP "app/sprites/Bazuca.bmp"

  
  let tiles = [grass, water, stone, worm, morto, barril, bazuca,sky]


  play janela fundo fr it (desenha tiles) reageEventos reageTempo
  where
    
    it = Menu 0
