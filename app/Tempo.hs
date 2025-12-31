{-|
Module      : Tempo
Description : Gestão temporal do jogo.

Módulo para a gestão do avanço temporal no jogo Worms usando a biblioteca Gloss.
-}
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

-- | Intervalo de tempo entre cada passo automático do jogo (em segundos).
--
-- __NB:__ Define a velocidade base de simulação. Valores menores aumentam a velocidade do jogo.
intervalo :: Segundos
intervalo = 1

-- * Função Principal

{-| Função principal que avança o tempo no estado do jogo no Gloss.

Funcionamento:

* Acumula o tempo decorrido desde o último tick
* Calcula quantos passos devem ser executados
* Aplica as mecânicas específicas de cada modo de jogo
* Atualiza o estado visual para renderização

Para cada modo de jogo:

== __Bot Simulation:__

* Gera jogadas táticas com 'jogadaTatica'
* Aplica jogadas ao estado com 'efetuaJogada' e 'avancaEstado'
* Mantém histórico da última jogada executada

== __Player vs Player (PVP):__

* Verifica condições de vitória por equipa
* Congela dinamites no chão antes de avançar o estado
* Aplica física e danos com 'avancaEstado'
* Retorna ecrã de Game Over quando uma equipa vence

== __Outros Modos:__

* Menu, Help, Quit, LevelSelector: Mantêm o estado inalterado
* MapCreatorTool: Preserva todas as configurações do editor

==__Exemplo de Utilização:__

@
-- No loop principal do Gloss
playIO janela fundo fr estadoInicial desenha reageEventos reageTempo
@

>>> reageTempo 0.5 (BotSimulation estado 0.3 5 ultimaJogada)
BotSimulation novoEstado 0.8 5 novaJogada

>>> reageTempo 1.2 (PVP estado 0.8 10 jogada)
PVP estadoAvancado 0.0 10 jogada

-}
reageTempo :: Segundos -> Worms -> IO Worms
reageTempo _ m@Menu{} = return m
reageTempo _ (Help p) = return (Help p)
reageTempo _ (Quit sel) = return (Quit sel)
reageTempo _ (LevelSelector i ei) = return (LevelSelector i ei) 
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



reageTempo _ (MapCreatorTool mp i a secSel thirdSel edit char minh disp) = return (MapCreatorTool mp i a secSel thirdSel edit char minh disp)
reageTempo _ MapSelector = return MapSelector
reageTempo _ (GameOver team) = return (GameOver team)


-- * Funções Auxiliares

{-| Congela dinamites que estão no chão para prevenir movimento indesejado.

Funcionamento:

* Percorre todos os objetos do estado
* Identifica disparos do tipo 'DinamiteDLC'
* Verifica se a dinamite está apoiada em terreno sólido
* Mantém a dinamite na mesma posição se estiver no chão

Esta função resolve um problema de física onde dinamites no chão
poderiam ser afetadas pela gravidade durante 'avancaEstado'.

==__Exemplos:__

@
estadoComDinamite = EstadoDLC mapa [DisparoDLC (2,3) Norte DinamiteDLC (Just 2) 0] minhocas Nothing 0 []
@

>>> congelaDinamitesNoChao estadoComDinamite
EstadoDLC mapa [DisparoDLC (2,3) Norte DinamiteDLC (Just 2) 0] minhocas Nothing 0 []

-}
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