module Main where

import Labs2025
import Tarefa1
import Magic



-- * Comandos para testar (rodar na cmd - bash - na pasta do ficheiro)
-- * cabal clean && rm -rf t1-feedback.tix
-- * cabal run --enable-coverage t1-feedback 
-- * ./runhpc.sh t1-feedback



-- | Definir aqui os testes do grupo para a Tarefa 1
testesTarefa1 :: [Estado]
testesTarefa1 = todosEstados

dataTarefa1 :: IO TaskData
dataTarefa1 = do
    let ins = testesTarefa1
    outs <- mapM (runTest . validaEstado) ins
    return $ T1 ins outs

main :: IO ()
main = runFeedback =<< dataTarefa1
