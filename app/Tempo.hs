module Tempo where

import Worms ( Worms(..) )
import Labs2025
    ( NumMinhoca, Direcao(Sul), Posicao, Estado, Jogada )
import Tarefa0_geral ( encontraPosicaoMatriz, movePosicao )
import Tarefa0_2025 ()
import Tarefa2 ( efetuaJogada )
import Tarefa3 ( avancaEstado )
import Tarefa4 ( jogadaTatica )
import DataDLC
    ( EstadoDLC(objetosEstadoDLC, minhocasEstadoDLC, mapaEstadoDLC),
      MapaDLC,
      MinhocaDLC(equipaMinhoca),
      ObjetoDLC(DisparoDLC),
      Team(Red, Blue),
      TerrenoDLC(PedraDLC, TerraDLC),
      TipoArmaDLC(DinamiteDLC) )
import AvancaEstado ( avancaEstado )
import EfetuaJogada ()
import Auxiliar ( getMinhocasValidasDLC )

type Segundos = Float

-- | Intervalo entre passos automático.
intervalo :: Segundos
intervalo = 1

-- | Função que avança o tempo no estado do jogo no Gloss.
reageTempo :: Segundos -> Worms -> IO Worms
reageTempo _ m@Menu{} = return m
reageTempo _ Help = return Help
reageTempo _ Quit = return Quit
reageTempo _ (LevelSelector i) = return (LevelSelector i) 
reageTempo dt (BotSimulation est acc tick ultimaJogada) = return $ BotSimulation estFinal acc' tick' novaJogada
        where
                acc2 = acc + dt
                steps = floor (acc2 / intervalo)
                acc' = acc2 - fromIntegral steps * intervalo
                tick' = tick + steps

                (estFinal, novaJogada) = aplicaPassos est tick steps ultimaJogada

                aplicaPassos :: Estado -> Int -> Int -> (NumMinhoca, Jogada) -> (Estado, (NumMinhoca, Jogada))
                aplicaPassos st _ 0 lastJogada = (st, lastJogada)
                aplicaPassos st t n _ = aplicaPassos estNovo (t + 1) (n - 1) jogadaAtual
                        where
                                (estNovo, jogadaAtual) = aplicaUm t st

                aplicaUm :: Int -> Estado -> (Estado, (NumMinhoca, Jogada))
                aplicaUm t st = (Tarefa3.avancaEstado $ Tarefa2.efetuaJogada jogador jogada st, (jogador, jogada))
                        where
                                (jogador, jogada) = jogadaTatica t st
                                

reageTempo _ (PVP est acc tick jogadaUser) = case (null minhocasRed, null minhocasBlue) of
        (True, False) -> return $ GameOver Blue  -- Blue venceu
        (False, True) -> return $ GameOver Red   -- Red venceu
        (True, True)  -> return $ GameOver Red   -- Empate (ou escolhe uma equipa)
        _             -> return $ PVP estFinal acc tick jogadaUser
  where
    minhocasVivas = getMinhocasValidasDLC (minhocasEstadoDLC est)
    minhocasRed = filter (\m -> equipaMinhoca m == Just Red) minhocasVivas
    minhocasBlue = filter (\m -> equipaMinhoca m == Just Blue) minhocasVivas

    estComDinamitesCongeladas = congelaDinamitesNoChao est

    estFinal = AvancaEstado.avancaEstado estComDinamitesCongeladas



reageTempo _ (MapCreatorTool mp i a l) = return (MapCreatorTool mp i a l)
reageTempo _ MapSelector = return MapSelector
reageTempo _ (GameOver team) = return (GameOver team)



congelaDinamitesNoChao :: EstadoDLC -> EstadoDLC
congelaDinamitesNoChao e = e { objetosEstadoDLC = map congelar (objetosEstadoDLC e) }
  where
    mapa = mapaEstadoDLC e
    
    congelar :: ObjetoDLC -> ObjetoDLC
    congelar obj@(DisparoDLC pos _ DinamiteDLC _ _)
      | estaNoChao pos mapa = obj  -- já está no chão, avancaEstado não deve mover
      | otherwise = obj  -- deixa avancaEstado aplicar gravidade
    congelar obj = obj
    
    estaNoChao :: Posicao -> MapaDLC -> Bool
    estaNoChao pos m = 
      let posAbaixo = Tarefa0_geral.movePosicao Sul pos
      in case Tarefa0_geral.encontraPosicaoMatriz posAbaixo m of
           Just TerraDLC -> True
           Just PedraDLC -> True
           _ -> False

