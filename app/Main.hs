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
fr = 60

path :: String
path = "app/sprites/"

main :: IO ()
main = do
  putStrLn "Iniciando Worms - demo"

  Just grass  <- loadJuicy "app/sprites/grass1.png"
  Just water  <- loadJuicy "app/sprites/water.png"
  Just stone  <- loadJuicy "app/sprites/stone1.png"
  Just sky    <- loadJuicy "app/sprites/sky.png"


  Just worm   <- loadJuicy "app/sprites/Worm.png"
  Just  morto  <- loadJuicy "app/sprites/Dead.png"
  Just  barril <- loadJuicy "app/sprites/Barril.png"
  Just  bazuca <- loadJuicy "app/sprites/Bazuca.png"
  Just  dinamite <- loadJuicy "app/sprites/Dinamite.png"
  Just  mina <- loadJuicy "app/sprites/Mina.png"
  
  let tiles = [grass, water, stone, worm, morto, barril, bazuca, sky, dinamite, mina]
  let temanatal = [grass, water, stone, worm, morto, barril, bazuca, sky, dinamite, mina]

  playIO janela fundo fr it (desenha tiles) reageEventos reageTempo
  where
    
    it = Menu 0
