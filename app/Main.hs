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


  play janela fundo fr it desenha reageEventos reageTempo
  where
    
    it = Menu 0
