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
    -- Congela dinamites que já estão no chão ANTES de avançar
    estComDinamitesCongeladas = congelaDinamitesNoChao est
    -- Agora avança
    estFinal = AvancaEstado.avancaEstado estComDinamitesCongeladas



reageTempo _ (MapCreatorTool mp i a) = return (MapCreatorTool mp i a)
reageTempo _ MapSelector = return MapSelector


congelaDinamitesNoChao :: EstadoDLC -> EstadoDLC
congelaDinamitesNoChao e = e { objetosEstadoDLC = map congelar (objetosEstadoDLC e) }
  where
    mapa = mapaEstadoDLC e
    
    congelar :: ObjetoDLC -> ObjetoDLC
    congelar obj@(DisparoDLC pos dir DinamiteDLC tempo dono)
      | estaNoChao pos mapa = obj  -- já está no chão, avancaEstado não deve mover
      | otherwise = obj  -- deixa avancaEstado aplicar gravidade
    congelar obj = obj
    
    estaNoChao :: Posicao -> MapaDLC -> Bool
    estaNoChao pos mapa = 
      let posAbaixo = movePosicao Sul pos
      in case encontraPosicaoMatriz posAbaixo mapa of
           Just TerraDLC -> True
           Just PedraDLC -> True
           _ -> False