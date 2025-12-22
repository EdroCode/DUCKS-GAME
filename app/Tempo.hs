module Tempo where

import Worms
import Labs2025
import Tarefa0_geral
import Tarefa0_2025
import Tarefa2
import Tarefa3
import Tarefa4
import DataDLC
import AvancaEstado
import EfetuaJogada


type Segundos = Float

-- | Intervalo entre passos automático.
intervalo :: Segundos
intervalo = 1

-- | Função que avança o tempo no estado do jogo no Gloss.
reageTempo :: Segundos -> Worms -> IO Worms
reageTempo _ m@Menu{} = return m
reageTempo _ Help = return Help
reageTempo _ Quit = return Quit
reageTempo dt (BotSimulation est acc tick ultimaJogada) = return $ BotSimulation estFinal acc' tick' novaJogada
        where
                acc2 = acc + dt
                steps = floor (acc2 / intervalo)
                acc' = acc2 - fromIntegral steps * intervalo
                tick' = tick + steps

                (estFinal, novaJogada) = aplicaPassos est tick steps ultimaJogada

                aplicaPassos :: Estado -> Int -> Int -> (NumMinhoca, Jogada) -> (Estado, (NumMinhoca, Jogada))
                aplicaPassos st _ 0 lastJogada = (st, lastJogada)
                aplicaPassos st t n lastJogada = aplicaPassos estNovo (t + 1) (n - 1) jogadaAtual
                        where
                                (estNovo, jogadaAtual) = aplicaUm t st

                aplicaUm :: Int -> Estado -> (Estado, (NumMinhoca, Jogada))
                aplicaUm t st = (Tarefa3.avancaEstado $ Tarefa2.efetuaJogada jogador jogada st, (jogador, jogada))
                        where
                                (jogador, jogada) = jogadaTatica t st
                                
reageTempo dt (PVP est acc tick jogadaUser) = return $ PVP estFinal acc tick jogadaUser
  where
    -- Primeiro avança o estado normalmente
    estAvancado = AvancaEstado.avancaEstado est
    
    -- Depois corrige as dinamites que deveriam estar paradas
    estFinal = corrigeDinamites estAvancado
    
    -- Corrige a posição das dinamites que deveriam estar paradas
    corrigeDinamites :: EstadoDLC -> EstadoDLC
    corrigeDinamites e = e { objetosEstadoDLC = map corrigeDinamite (objetosEstadoDLC e) }
      where
        mapa = mapaEstadoDLC e
        
        corrigeDinamite :: ObjetoDLC -> ObjetoDLC
        corrigeDinamite obj@(DisparoDLC pos dir DinamiteDLC tempo dono) =
          -- Verifica se há terreno sólido abaixo
          let posAbaixo = movePosicao Sul pos
              terrenoAbaixo = encontraPosicaoMatriz posAbaixo mapa
              deveParar = case terrenoAbaixo of
                Just TerraDLC -> True
                Just PedraDLC -> True
                _ -> False
          in if deveParar
             then obj  -- Mantém a posição atual
             else obj  -- Deixa o avancaEstado lidar com outros casos
        corrigeDinamite obj = obj
reageTempo _ (MapCreatorTool mp i a) = return (MapCreatorTool mp i a)
reageTempo _ MapSelector = return MapSelector
