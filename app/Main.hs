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
--Tiles
  grass <- loadBMP "app/sprites/Grass_1_16x16.bmp" -- 0
  water <- loadBMP "app/sprites/Water_1_16x16.bmp" -- 1
  stone <- loadBMP "app/sprites/Stone_1_16x16.bmp" -- 2
  sky <- loadBMP "app/sprites/Ice_18_16x16.bmp" -- 8
--Minhocas e objetos
  worm <- loadBMP "app/sprites/Worm.bmp" -- 3
  morto <- loadBMP "app/sprites/Dead.bmp" -- 4
  barril <- loadBMP "app/sprites/Barril.bmp" -- 5
  bazuca <- loadBMP "app/sprites/Bazuca.bmp" -- 6
  dinamite <- loadBMP "app/sprites/Dinamite.bmp" -- 7
  mina <- loadBMP "app/sprites/Mina.bmp" -- 9
  
  let tiles = [grass, water, stone, worm, morto, barril, bazuca, dinamite, sky, mina]

  play janela fundo fr it (desenha tiles) reageEventos reageTempo
  where
    
    it = Menu 0
