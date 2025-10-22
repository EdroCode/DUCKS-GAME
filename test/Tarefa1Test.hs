module Main where

import Labs2025
import Tarefa1
import Magic

-- * Comandos para testar (rodar na cmd - bash - na pasta do ficheiro)
-- * cabal clean && rm -rf t1-feedback.tix
-- * cabal run --enable-coverage t1-feedback ./runhpc.sh t1-feedback




m = [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
    ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
    ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
    ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
    ,[Terra,Terra,Terra,Terra,Terra,Ar,Ar,Ar,Ar,Ar]
    ,[Terra,Terra,Terra,Terra,Terra,Pedra,Pedra,Agua,Agua,Agua]
    ]

barrilTeste = Barril
  { posicaoBarril = (3,2)
  , explodeBarril = False
  }

disparo1 = Disparo
  { posicaoDisparo = (1,4)
  , direcaoDisparo = Norte
  , tipoDisparo = Mina
  , tempoDisparo = Just 2
  , donoDisparo = 0
}

disparo2 = Disparo
  { posicaoDisparo = (2,4)
  , direcaoDisparo = Norte
  , tipoDisparo = Dinamite
  , tempoDisparo = Just 2
  , donoDisparo = 1
}

minhoca1 = Minhoca{
    posicaoMinhoca = Just(3,0),
    vidaMinhoca = Morta,
    jetpackMinhoca = 100,
    escavadoraMinhoca = 200,
    bazucaMinhoca = 150,
    minaMinhoca = 3,
    dinamiteMinhoca = 1
    }

minhoca2 = Minhoca{
    posicaoMinhoca = Just(3,1),
    vidaMinhoca = Viva 0,
    jetpackMinhoca = 100,
    escavadoraMinhoca = 200,
    bazucaMinhoca = 150,
    minaMinhoca = 3,
    dinamiteMinhoca = 1
    }

minhocas = [minhoca1, minhoca2]
objetos = [disparo1, disparo2, barrilTeste]


estado1 = Estado
  { mapaEstado     = m
  , objetosEstado  = objetos
  , minhocasEstado = minhocas
  }





-- | Definir aqui os testes do grupo para a Tarefa 1
testesTarefa1 :: [Estado]
testesTarefa1 = [estado1]

dataTarefa1 :: IO TaskData
dataTarefa1 = do
    let ins = testesTarefa1
    outs <- mapM (runTest . validaEstado) ins
    return $ T1 ins outs

main :: IO ()
main = runFeedback =<< dataTarefa1
