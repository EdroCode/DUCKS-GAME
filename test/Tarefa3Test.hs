module Main where

import Labs2025
import Tarefa3
import Magic




mapaOk = [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
    ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
    ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
    ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
    ,[Terra,Terra,Terra,Terra,Terra,Ar,Ar,Ar,Ar,Ar]
    ,[Terra,Terra,Terra,Terra,Terra,Pedra,Pedra,Agua,Agua,Agua]
    ]



-- * -----------------------------------------
-- * OBJETOS
-- * -----------------------------------------

-- DISPAROS VÁLIDOS
disparoValido1 = Disparo{posicaoDisparo=(1,4), direcaoDisparo=Norte, tipoDisparo=Mina, tempoDisparo=Just 2, donoDisparo=0}
disparoValido2 = Disparo{posicaoDisparo=(3,9), direcaoDisparo=Sul, tipoDisparo=Dinamite, tempoDisparo=Just 4, donoDisparo=1}
disparoValido3 = Disparo{posicaoDisparo=(3,3), direcaoDisparo=Este, tipoDisparo=Bazuca, tempoDisparo=Nothing, donoDisparo=0}
disparoValido4 = Disparo{posicaoDisparo=(2,3), direcaoDisparo=Norte, tipoDisparo=Bazuca, tempoDisparo=Nothing, donoDisparo=1} -- perfura terreno opaco se posição anterior não for opaca
disparoValido5 = Disparo{posicaoDisparo=(0,0), direcaoDisparo=Sul, tipoDisparo=Mina, tempoDisparo=Just 0, donoDisparo=3}
disparoValido7 = Disparo{posicaoDisparo=(3,1), direcaoDisparo=Oeste, tipoDisparo=Dinamite, tempoDisparo=Just 2, donoDisparo=0}


-- BARRIS VÁLIDOS
barrilValido1 = Barril{posicaoBarril=(4,6),explodeBarril = False }
barrilValido2 = Barril{posicaoBarril=(0,5),explodeBarril = False }
barrilValido3 = Barril{posicaoBarril=(2,0),explodeBarril = False }



-- * -----------------------------------------
-- * MINHOCAS
-- * -----------------------------------------

-- MINHOCAS VÁLIDAS
minhocaValida1 = Minhoca{posicaoMinhoca=Just (3,0), vidaMinhoca=Morta, jetpackMinhoca=100, escavadoraMinhoca=200, bazucaMinhoca=150, minaMinhoca=3, dinamiteMinhoca=1} -- posição válida, morta, munições >= 0
minhocaValida2 = Minhoca{posicaoMinhoca=Just (1,1), vidaMinhoca=Viva 50, jetpackMinhoca=0, escavadoraMinhoca=0, bazucaMinhoca=0, minaMinhoca=0, dinamiteMinhoca=0} -- viva, vida entre 0-100, posição livre
minhocaValida3 = Minhoca{posicaoMinhoca=Nothing, vidaMinhoca=Morta, jetpackMinhoca=10, escavadoraMinhoca=5, bazucaMinhoca=2, minaMinhoca=0, dinamiteMinhoca=0} -- sem posição, obrigatoriamente morta
minhocaValida4 = Minhoca{posicaoMinhoca=Just (2,2), vidaMinhoca=Viva 0, jetpackMinhoca=0, escavadoraMinhoca=0, bazucaMinhoca=0, minaMinhoca=0, dinamiteMinhoca=0} -- viva com vida 0 (permitido), posição válida
minhocaValida5 = Minhoca{posicaoMinhoca=Just (0,0), vidaMinhoca=Viva 100, jetpackMinhoca=50, escavadoraMinhoca=50, bazucaMinhoca=50, minaMinhoca=5, dinamiteMinhoca=2} -- posição válida, vida máxima


-- * -----------------------------------------
-- * ESTADOS
-- * -----------------------------------------

estado1 = Estado{mapaEstado=mapaOk, objetosEstado=[disparoValido1, disparoValido2, barrilValido1], minhocasEstado=[minhocaValida1, minhocaValida2]} -- estado totalmente válido

estado26 = Estado{mapaEstado=mapaOk, objetosEstado=[disparoValido2, disparoValido3], minhocasEstado=[minhocaValida3, minhocaValida4]} -- minhoca sem posição + viva com vida 0


estado33 = Estado{mapaEstado=mapaOk, objetosEstado=[disparoValido7, barrilValido1], minhocasEstado=[minhocaValida2]} -- múltiplos disparos diferentes + barril válido



estado38 = Estado{mapaEstado=mapaOk, objetosEstado=[disparoValido3, disparoValido4, barrilValido2], minhocasEstado=[minhocaValida5, minhocaValida1]} -- combinação múltipla de disparos válidos e barril, minhocas válidas


-- * -----------------------------------------
-- * LISTA DE TODOS OS ESTADOS
-- * -----------------------------------------

todosEstados = [
    estado1,
    estado26,
    estado33,
    estado38]



-- | Definir aqui os testes do grupo para a Tarefa 3
testesTarefa3 :: [Estado]
testesTarefa3 = todosEstados

dataTarefa3 :: IO TaskData
dataTarefa3 = do
    let ins = testesTarefa3
    outs <- mapM (runTest . avancaEstado) ins
    return $ T3 ins outs

main :: IO ()
main = runFeedback =<< dataTarefa3