module Main where

import Labs2025
import Tarefa3
import Magic
import OracleT3

-- * Comandos para testar (rodar na cmd - bash - na pasta do ficheiro)
-- * cabal clean && rm -rf t3-feedback.tix
-- * cabal run --enable-coverage t3-feedback 
-- * ./runhpc.sh t3-feedback




mapaValido = [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Terra,Pedra,Agua,Agua,Agua,Agua,Agua]
        ,[Terra,Terra,Terra,Terra,Pedra,Pedra,Agua,Agua,Agua,Agua]
        ,[Terra,Terra,Terra,Terra,Terra,Pedra,Pedra,Pedra,Agua,Agua]]


-- * OBJETOS

barril1 = Barril {posicaoBarril = (2,0), explodeBarril = True}
barril2 = Barril {posicaoBarril = (0,0), explodeBarril = False}

bazuca1 = Disparo {posicaoDisparo = (0,5), direcaoDisparo = Este, tipoDisparo = Bazuca, tempoDisparo = Nothing, donoDisparo = 0}
bazuca2 = Disparo {posicaoDisparo = (2,3), direcaoDisparo = Sul, tipoDisparo = Bazuca, tempoDisparo = Nothing, donoDisparo = 0}
        

mina1 = Disparo {posicaoDisparo = (2,5), direcaoDisparo = Norte, tipoDisparo = Mina, tempoDisparo = Just 2, donoDisparo = 0}
mina2 = Disparo {posicaoDisparo = (3,5), direcaoDisparo = Sul, tipoDisparo = Mina, tempoDisparo = Just 2, donoDisparo = 0}

dinamite1 = Disparo {posicaoDisparo = (2,3), direcaoDisparo = Nordeste, tipoDisparo = Dinamite, tempoDisparo = Just 3, donoDisparo = 0}
dinamite2 = Disparo {posicaoDisparo = (2,7), direcaoDisparo = Sul, tipoDisparo = Dinamite, tempoDisparo = Just 3, donoDisparo = 0}
dinamite3 = Disparo {posicaoDisparo = (4,9), direcaoDisparo = Nordeste, tipoDisparo = Dinamite, tempoDisparo = Just 3, donoDisparo = 0}
               

-- * MINHOCAS

minhoca1 = Minhoca {posicaoMinhoca = Just (2,1), vidaMinhoca = Viva 100, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}
minhoca2 = Minhoca {posicaoMinhoca = Just (1,8), vidaMinhoca = Viva 100, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}
minhoca3 = Minhoca {posicaoMinhoca = Just (3,8), vidaMinhoca = Morta, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}
minhoca4 = Minhoca {posicaoMinhoca = Just (1,3), vidaMinhoca = Viva 0, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}
        
-- * ESTADOS


estado26 = Estado
    { mapaEstado =
        [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Terra,Agua,Agua,Agua]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Agua,Agua]
        ]
    , objetosEstado =
        [Disparo {posicaoDisparo = (2,5), direcaoDisparo = Noroeste, tipoDisparo = Dinamite, tempoDisparo = Just 0, donoDisparo = 0}
        ]
    , minhocasEstado =
        [Minhoca {posicaoMinhoca = Just (2,3), vidaMinhoca = Viva 100, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}
        ]
    }




dataTarefa3 :: IO TaskData
dataTarefa3 = do
    let ins = testesTarefa3
    outs <- mapM (runTest . avancaEstado) ins
    return $ T3 ins outs

main :: IO ()
main = runFeedback =<< dataTarefa3

-- TOdo justificar no haddock o estado20