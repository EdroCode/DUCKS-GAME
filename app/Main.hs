module Main where

import Graphics.Gloss

import Desenhar
import Eventos
import Worms
import Tempo




janela :: Display
janela = InWindow "Worms" (1920, 1080) (0, 0)

fundo :: Color
fundo = white


fr :: Int
fr = 60


main :: IO ()
main = do
  putStrLn "Iniciando Worms - demo"

  grass <- loadBMP "app/sprites/Grass_1_16x16.bmp"
  water <- loadBMP "app/sprites/Water_1_16x16.bmp"
  stone <- loadBMP "app/sprites/Stone_1_16x16.bmp"
  worm <- loadBMP "app/sprites/Worm.bmp"
  morto <- loadBMP "app/sprites/Dead.bmp"
  barril <- loadBMP "app/sprites/Barril.bmp"
  bazuca <- loadBMP "app/sprites/Bazuca.bmp"
  let tiles = [grass, water, stone, worm, morto, barril, bazuca]

  play janela fundo fr it (desenha tiles) reageEventos reageTempo
  where
    
    it = Menu 0
