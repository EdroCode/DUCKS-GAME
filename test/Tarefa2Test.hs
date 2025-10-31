module Main where

import Labs2025
import Tarefa2
import Magic


jogadaTeste2 = (0,Move Oeste,estadoTeste)
jogadaTeste3 = (0,Move Sul,estadoTeste)
jogadaTeste4 = (0,Move Sudoeste,estadoTeste)


-- | Definir aqui os testes do grupo para a Tarefa 2
testesTarefa2 :: [(NumMinhoca,Jogada,Estado)]
testesTarefa2 = [jogadaTeste2,jogadaTeste3,jogadaTeste4]

dataTarefa2 :: IO TaskData
dataTarefa2 = do
    let ins = testesTarefa2
    outs <- mapM (\(i,j,e) -> runTest $ efetuaJogada i j e) ins
    return $ T2 ins outs

main :: IO ()
main = runFeedback =<< dataTarefa2
