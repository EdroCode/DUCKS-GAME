module Main where

import Labs2025
import Tarefa2
import Magic
import JogadasArquivo
import OracleT2
import Labs2025 (Estado(armaSelecionada), TipoArma (Escavadora))
import Data.Maybe (Maybe(Nothing))








dataTarefa2 :: IO TaskData
dataTarefa2 = do
    let ins = (0 :: Int, Dispara Escavadora Oeste, olaaaa)
    outs <- mapM (\(i,j,e) -> runTest $ efetuaJogada i j e) ins
    return $ T2 ins outs

main :: IO ()
main = runFeedback =<< dataTarefa2
