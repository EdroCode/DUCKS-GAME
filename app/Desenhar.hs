module Desenhar where

import Graphics.Gloss

import Worms
import Labs2025
import Tarefa0_geral
import Tarefa0_2025

-- | Menu do jogo
desenha :: Worms -> Picture
desenha (Menu sel) = drawMenu sel
desenha (Playing est _ _) = drawGame est
desenha Quit = Translate (-50) 0 $ Scale 0.5 0.5 $ Text "Aperte ESC para confirmar saída."
desenha Help = drawHelp


drawMenu :: Int -> Picture
drawMenu sel = Pictures
	[ Translate (-220) 120 $ Scale 0.6 0.6 $ Color black $ Text "WORMS"
	, Translate (-220) 10 $ Scale 0.35 0.35 $ Color (if sel==0 then red else black) $ Text "Start Game"
	, Translate (-220) (-40) $ Scale 0.35 0.35 $ Color (if sel==1 then red else black) $ Text "Help"
	, Translate (-220) (-90) $ Scale 0.35 0.35 $ Color (if sel==2 then red else black) $ Text "Quit"
	]

-- | Tela de ajuda / instruções
drawHelp :: Picture
drawHelp = Pictures
	[ Translate (-360) 240 $ Scale 0.5 0.5 $ Color black $ Text "Help / Instruções"
	, Translate (-360) 120 $ Scale 0.25 0.25 $ Color black $ Text "↑/↓ - navegar no menu"
	, Translate (-360) 80 $ Scale 0.25 0.25 $ Color black $ Text "Enter - seleccionar (Start / Help / Quit)"
	, Translate (-360) (-120) $ Scale 0.18 0.18 $ Color (greyN 0.5) $ Text "Pressione ESC ou Enter para voltar ao menu"
	]

cellSize :: Float
cellSize = 48

drawGame :: Estado -> Picture
-- Mostra uma linha de status e, ao centro, o mundo (mapa + objetos + minhocas)
drawGame est = Pictures [Translate (-640) 240 $ Scale 0.12 0.12 $ Color black $ Text infoMapa, Translate (-150) 0 world]
	where
		mapa = mapaEstado est
		infoMapa = "mapa: " ++ show (length mapa) ++ "x" ++ show (if null mapa then 0 else length (head mapa))
		objs = objetosEstado est
		ms = minhocasEstado est
		world = Pictures [drawMapa mapa, drawObjetos objs mapa, drawMinhocas ms mapa]


-- | Converte coordenadas do mapa (linha, coluna) para coordenadas em pixels
-- centrar o mapa na janela, e cada célula tem tamanho 48(cellSize).
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

-- | Desenha todas as células do mapa (cada tile com uma cor simples)
drawMapa :: Mapa -> Picture
drawMapa mapa = Pictures $ concatMap drawRow (zip [0..] mapa)
	where
		drawRow (r, row) = map (drawTile r) (zip [0..] row)
		drawTile r (c, t) = Translate x y $ Pictures [colorTile t, Color (greyN 0.6) $ rectangleWire cellSize cellSize]
			where
				(x,y) = converteMapa mapa (r,c)
				colorTile Ar = Color (greyN 0.95) $ rectangleSolid cellSize cellSize
				colorTile Agua = Color (makeColor 0 0.4 1 0.6) $ rectangleSolid cellSize cellSize
				colorTile Terra = Color (makeColor 0.6 0.4 0.2 1) $ rectangleSolid cellSize cellSize
				colorTile Pedra = Color (greyN 0.5) $ rectangleSolid cellSize cellSize

-- | Desenha objetos no mapa (disparos, minas, dinamites, barris, ...)
drawObjetos :: [Objeto] -> Mapa -> Picture
drawObjetos objs mapa = Pictures $ map drawO objs
	where
		drawO o@(Disparo {}) = Translate x y $ case tipoDisparo o of
			Bazuca -> Pictures [Color red $ circleSolid (cellSize * 0.25), Color black $ Scale 0.15 0.15 $ Text (show (donoDisparo o))]
			Mina -> Color (makeColor 0.9 0.7 0 1) $ circleSolid (cellSize * 0.18)
			Dinamite -> Color (makeColor 0.9 0.2 0.2 1) $ rectangleSolid (cellSize * 0.3) (cellSize * 0.3)
			_ -> Color black $ circleSolid (cellSize * 0.12)
			where (x,y) = converteMapa mapa (posicaoObjeto o)

		drawO b@(Barril {}) = Translate x y $ Pictures [Color (makeColor 0.3 0.15 0 1) $ rectangleSolid (cellSize*0.4) (cellSize*0.4), Color black $ rectangleWire (cellSize*0.4) (cellSize*0.4)]
			where (x,y) = converteMapa mapa (posicaoObjeto b)

-- | Desenha as minhocas. Cada minhoca aparece com um círculo colorido
-- e um rótulo que indica o índice e a vida restante.
drawMinhocas :: [Minhoca] -> Mapa -> Picture
drawMinhocas ms mapa = Pictures $ map drawM (zip [0..] ms)
	where
		drawM (i,m) = case posicaoMinhoca m of
			Nothing -> Blank
			Just p -> Translate x y $ Pictures [Color (cor m) $ circleSolid (cellSize * 0.28), Color black $ Scale 0.12 0.12 $ Text (label i m)]
				where
					(x,y) = converteMapa mapa p
		cor m = case vidaMinhoca m of
			Morta -> makeColor 0 0 0 1
			Viva v -> if v <= 25 then makeColor 1 0.3 0 1 else makeColor 0 0.8 0 1
		label i m = "M" ++ show i ++ " " ++ case vidaMinhoca m of {Morta -> "MORTA"; Viva n -> show n}

