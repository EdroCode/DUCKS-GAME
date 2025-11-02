module Main where

import Labs2025
import Tarefa2
import Magic


estadoTeste2 = Estado
    { mapaEstado = mapaOk
    , objetosEstado =
        []
    , minhocasEstado =
        [Minhoca {posicaoMinhoca = Just (3,0), vidaMinhoca = Viva 100, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}]
    }



jogadaTeste22 = (0,Dispara Escavadora Sudeste,estadoTeste)
jogadaTeste3 = (0,Move Sul,estadoTeste)
jogadaTeste4 = (0,Move Sudeste,estadoTeste)
jogadaTeste5 = (0,Dispara Bazuca Norte,estadoTeste)
jogadaTeste61 = (0,Dispara Bazuca Oeste,estadoTeste2)

jogadaTeste7 = (0,Dispara Mina Sul,estadoTeste)
jogadaTeste8 = (0,Dispara Mina Norte,estadoTeste)
jogadaTeste9 = (0,Dispara Dinamite Norte,estadoTeste)
jogadaTeste10 = (0,Dispara Dinamite Oeste,estadoTeste2)






-- | Definir aqui os testes do grupo para a Tarefa 2
testesTarefa2 :: [(NumMinhoca,Jogada,Estado)]
testesTarefa2 = [jogadaTeste22,jogadaTeste3,jogadaTeste4,jogadaTeste5, jogadaTeste61, jogadaTeste7, jogadaTeste8, jogadaTeste9, jogadaTeste10]

dataTarefa2 :: IO TaskData
dataTarefa2 = do
    let ins = testesTarefa2
    outs <- mapM (\(i,j,e) -> runTest $ efetuaJogada i j e) ins
    return $ T2 ins outs

main :: IO ()
main = runFeedback =<< dataTarefa2
