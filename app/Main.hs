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
fr = 30

path :: String
path = "app/sprites/"



main :: IO ()
main = do
  putStrLn "Iniciando Worms - demo"



 -- * Base game

  
  Just grass  <- loadJuicy (path ++"grass1.png")
  Just water  <- loadJuicy (path ++"water.png")
  Just stone  <- loadJuicy (path ++"stone1.png")
  Just sky    <- loadJuicy (path ++"sky.png")
  Just dirt  <- loadJuicy (path ++"Terra.png")

  Just worm   <- loadJuicy (path ++"Worm.png")
  Just  morto  <- loadJuicy (path ++"Dead.png")
  Just  barril <- loadJuicy (path ++"Barril.png")
  Just  bazuca <- loadJuicy (path ++"Bazuca.png")
  Just  dinamite <- loadJuicy (path ++"Dinamite.png")
  Just  mina <- loadJuicy (path ++"Mina.png")


  -- * Extras
  
  let tiles = [grass, water, stone, worm, morto, barril, bazuca, sky, dinamite, mina, dirt]
  let temanatal = [grass, water, stone, worm, morto, barril, bazuca, sky, dinamite, mina]

  playIO 
    janela -- janela onde irá correr o jogo
    fundo -- côr do fundo da janela
    fr  -- frame rate
    it -- estado inicial
    (desenha tiles)  -- desenha o estado do jogo
    reageEventos  -- reage a um evento
    reageTempo -- reage ao passar do tempo

         
  where
    
    it = Menu 0


