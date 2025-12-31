{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-|
Module      : Desenhar
Description : Renderização visual do jogo Worms.

Módulo que contém funções para desenhar os elementos do jogo Worms usando a biblioteca Gloss.
-}
module Desenhar where

import Graphics.Gloss

import Worms
import Labs2025
import Tarefa0_2025
import Tarefa2
import Tarefa4 (getMinhocasValidas, getXWay)
import DataDLC
import Auxiliar (getMinhocasValidasDLC, eMinhocaVivaDLC)
import EfetuaJogada
import Data.Char (toLower)



-- * Constantes de Janela

-- | Largura da janela do jogo em pixels.

janelaLargura :: Float
janelaLargura = 1920

-- | Altura da janela do jogo em pixels.

janelaAltura :: Float
janelaAltura = 1080


-- * Tipos e Estruturas de Dados

{-| Estrutura que contém todos os recursos gráficos (sprites) do jogo.

Componentes:

* 'imgWorm': sprite da minhoca
* 'imgBarrel': sprite do barril
* 'imgBackground': imagem de fundo
* 'grassImg': textura de relva
* 'waterImg': textura de água
* 'stoneImg': textura de pedra
-}
data Assets = Assets
  { imgWorm :: Picture
  , imgBarrel :: Picture
  , imgBackground :: Picture
  , grassImg :: Picture
  , waterImg :: Picture
  , stoneImg :: Picture
  }

{-| Tuplo que representa o estado do jogo com os seus recursos gráficos.

Componentes:

* 'Estado': estado atual do jogo
* 'Assets': recursos gráficos carregados
-}
type EstadoGloss = (Estado, Assets)

-- * Função Principal de Renderização

{-| Função principal que coordena a renderização de todos os estados do jogo.

Funcionamento:

* Recebe uma lista de 'Picture' (recursos gráficos) e o estado atual 'Worms'
* Delega para a função de desenho apropriada conforme o estado
* Retorna uma 'Picture' pronta para renderização no Gloss

Estados suportados:

* 'Menu': menu principal do jogo
* 'BotSimulation': modo de simulação com bots
* 'PVP': modo jogador contra jogador
* 'MapCreatorTool': editor de mapas
* 'LevelSelector': seletor de níveis
* 'Quit': confirmação de saída
* 'Help': ecrã de ajuda
* 'GameOver': ecrã de fim de jogo

==__Exemplos de Utilização:__

>>> desenha recursos (Menu 0)
Picture (menu com primeira opção selecionada)

>>> desenha recursos (PVP estado 0.0 0 jogada)
Picture (jogo PvP renderizado)
-}
desenha :: [Picture] -> Worms -> IO Picture
desenha p (Menu sel) = return $ drawMenu p sel
desenha p (BotSimulation est _ _ (numMinhoca, jogada)) = return $ drawGame p est (Just numMinhoca) (Just jogada)
desenha p (PVP est _ _ jogada) = return $ drawPvPGame p est jogada
desenha p (MapCreatorTool e b a secSel thirdSel edit char worm disp) = return $ (drawMCT p e b a secSel thirdSel edit char worm disp)
desenha p (LevelSelector i estImp) = return $ (drawLvlSelector p i estImp)
desenha p (Quit sel) = return $ drawQuitConfirm p sel
desenha p (Help pagina) = return $ drawHelp p pagina
desenha p (GameOver team) = return $ drawGameOver p team

-- * Funções de Desenho de Menus

{-| Desenha a interface de confirmação de saída do jogo.

Funcionamento:

* Apresenta uma mensagem de confirmação
* Mostra dois botões: "Confirmar" e "Cancelar"
* Destaca o botão selecionado com base no parâmetro 'sel'

Parâmetros:

* @p@: lista de recursos gráficos ('Picture')
* @sel@: índice do botão selecionado (0 = Confirmar, 1 = Cancelar)

==__Exemplo de Utilização:__

>>> drawQuitConfirm recursos 0
Picture (botão Confirmar destacado)

>>> drawQuitConfirm recursos 1
Picture (botão Cancelar destacado)
-}
drawQuitConfirm :: [Picture] -> Int -> Picture
drawQuitConfirm p sel = Pictures
  [ p !! 88
  , Translate 50 350 $ Scale 1 1 $ p !! 89
  , Translate (-400) 350 $ Scale 1.2 1.2 $ Color black $ drawWord p "Deseja sair do jogo?"
  , Translate (-300) (-50) $ Scale 1 1 (if sel==0 then p !! 21 else p !! 22)
  , Translate 300 (-50) $ Scale 1 1 (if sel==1 then p !! 21 else p !! 22)
  , Translate (-460) (-50) $ Scale 1 1 $ drawWord p "Confirmar"
  , Translate 150 (-50) $ Scale 1 1 $ drawWord p "Cancelar"
  ]

{-| Desenha o menu principal do jogo.

Funcionamento:

* Apresenta o título "WORMS"
* Mostra 5 opções de menu dispostas em grid
* Destaca a opção selecionada
* Opções disponíveis: Bot Simulation, Player vs Player, MAP Creator Tool, Help, Quit

Parâmetros:

* @p@: lista de recursos gráficos ('Picture')
* @sel@: índice da opção selecionada (0-4)

==__Exemplo de Utilização:__

>>> drawMenu recursos 0
Picture (Bot Simulation selecionado)

>>> drawMenu recursos 4
Picture (Quit selecionado)
-}
drawMenu :: [Picture] -> Int -> Picture
drawMenu p sel = Pictures
  [ Scale 1 1 $ p !! 85

  -- Título "WORMS"
  , Translate 0 350 $ Scale 1 1 $ p !! 20
  , Translate (-140) 370 $ Scale 1.6 1.6 $ drawWord p "worms"

  -- Subtítulo
  , Translate (-210) 290 $ Scale 0.5 0.5 $ drawWord p "Escolha o modo de jogo"

  -- Bot Simulation (esquerda superior)
  , Translate (-300) 100 $ Scale 1 1 $ (if sel==0 then p !! 21 else p !! 22)
  , Translate (-300- 150) 100 $ Scale 0.6 0.6 $ Color (if sel==0 then red else black) $ drawWord p "Bot Simulation"

  -- Player vs Player (direita superior)
  , Translate 300 100 $ Scale 1 1 $ (if sel==1 then p !! 21 else p !! 22)
  , Translate (300- 160) 100 $ Scale 0.6 0.6 $ Color (if sel==1 then red else black) $ drawWord p "Player vs Player"

  -- MAP Creator Tool (esquerda inferior)
  , Translate (-300) (-50) $ Scale 1 1 $ (if sel==2 then p !! 21 else p !! 22)
  , Translate (-300- 160) (-50) $ Scale 0.6 0.6 $ Color (if sel==2 then red else black) $ drawWord p "MAP Creator Tool"

  -- Help (direita inferior)
  , Translate 300 (-50) $ Scale 1 1 $ (if sel==3 then p !! 21 else p !! 22)
  , Translate (300 - 50) (-50) $ Scale 0.6 0.6 $ Color (if sel==3 then red else black) $ drawWord p "Help"

  -- Quit (centro baixo)
  , Translate 0 (-200) $ Scale 1 1 $ (if sel==4 then p !! 21 else p !! 22)
  , Translate (- 50) (-200) $ Scale 0.6 0.6 $ Color (if sel==4 then red else black) $ drawWord p "Quit"
  ]


{-| Desenha o ecrã de ajuda do jogo com navegação entre páginas.

Funcionamento:

* Apresenta informações de ajuda divididas em 6 páginas
* Permite navegação entre páginas com indicadores visuais
* Mostra título, conteúdo e navegação para cada página

Páginas disponíveis:

1. Menu Principal - navegação no menu
2. Level Selector - seleção de níveis
3. Movimentação PVP - controlos de movimento
4. Controles PVP - ações e comandos
5. Map Creator 1 - uso básico do editor
6. Map Creator 2 - funcionalidades avançadas

Parâmetros:

* @p@: lista de recursos gráficos ('Picture')
* @pagina@: índice da página atual (0-5)

==__Exemplo de Utilização:__

>>> drawHelp recursos 0
Picture (página do Menu Principal)

>>> drawHelp recursos 3
Picture (página de Controles PVP)
-}

drawHelp :: [Picture] -> Int -> Picture
drawHelp p pagina = Pictures
  [ Translate (-100) 0 $ Scale 1.2 1.2 $ p !! 125
  , Translate (-800) 450 $ Scale 2 2 $ Color black $ drawWord p (titulosPaginas !! pagina)


  , conteudoPagina pagina


  , Translate (-50) (-450) $ Scale 0.6 0.6 $ Color (greyN 0.5) $ drawWord p ("Pagina " ++ show (pagina + 1) ++ " de " ++ show totalPaginas)


  , if pagina > 0
      then Translate (-320) (-450) $ Scale 0.6 0.6 $ Color (makeColor 0.2 0.4 0.8 1.0) $ drawWord p "< Anterior"
      else Blank

  , if pagina < totalPaginas - 1
      then Translate (300) (-450) $ Scale 0.6 0.6 $ Color (makeColor 0.2 0.4 0.8 1.0) $ drawWord p "Proximo >"
      else Blank

  , Translate (-800) (-500) $ Scale 0.6 0.6 $ Color (greyN 0.5) $ drawWord p "Use Setas Esq/Dir para navegar | ESC para voltar ao menu"
  ]
  where
    totalPaginas = length titulosPaginas

    titulosPaginas =
      [ "MENU PRINCIPAL"
      , "LEVEL SELECTOR"
      , "MOVIMENTACAO PVP"
      , "CONTROLES PVP"
      , "MAP CREATOR 1"
      , "MAP CREATOR 2"
      ]

    conteudoPagina 0 = Pictures
      [ Translate (-800) 300 $ Scale 1.5 1.5 $ Color (makeColor 0.2 0.4 0.8 1.0) $ drawWord p "MENU PRINCIPAL"
      , Translate (-800) 200 $ Scale 1 1 $ Color black $ drawWord p "Setas: Navegar entre opcoes"
      , Translate (-800) 140 $ Scale 1 1 $ Color black $ drawWord p "Enter: Selecionar opcao"
      , Translate (-800) 80 $ Scale 1 1 $ Color black $ drawWord p "ESC: Voltar/Sair"
      , Translate (-800) (-50) $ Scale 1.5 1.5 $ Color (greyN 0.4) $ drawWord p "Opcoes disponiveis:"
      , Translate (-800) (-100) $ Scale 1 1 $ Color black $ drawWord p "- Bot Simulation"
      , Translate (-800) (-140) $ Scale 1 1 $ Color black $ drawWord p "- Player vs Player"
      , Translate (-800) (-180) $ Scale 1 1 $ Color black $ drawWord p "- MAP Creator Tool"
      , Translate (-800) (-220) $ Scale 1 1 $ Color black $ drawWord p "- Help"
      , Translate (-800) (-260) $ Scale 1 1 $ Color black $ drawWord p "- Quit"
      ]

    conteudoPagina 1 = Pictures
      [ Translate (-800) 300 $ Scale 1.5 1.5 $ Color (makeColor 0.2 0.4 0.8 1.0) $ drawWord p "LEVEL SELECTOR"
      , Translate (-800) 200 $ Scale 1 1 $ Color black $ drawWord p "I: Importa mapa salvo"
      , Translate (-800) 140 $ Scale 1 1 $ Color black $ drawWord p "Setas Cima/Baixo: Navegar niveis"
      , Translate (-800) 80 $ Scale 1 1 $ Color black $ drawWord p "Enter: Jogar nivel selecionado"
      , Translate (-800) 20 $ Scale 1 1 $ Color black $ drawWord p "ESC: Voltar ao menu"
      ]

    conteudoPagina 2 = Pictures
      [ Translate (-800) 300 $ Scale 1.5 1.5 $ Color (makeColor 0.2 0.4 0.8 1.0) $ drawWord p "MODO PVP - MOVIMENTACAO"
      , Translate (-800) 200 $ Scale 1 1 $ Color (greyN 0.4) $ drawWord p "Movimento Base:"
      , Translate (-800) 150 $ Scale 1 1 $ Color black $ drawWord p "WASD ou Setas: Norte/Sul/Este/Oeste"
      , Translate (-800) 70 $ Scale 1 1 $ Color (greyN 0.4) $ drawWord p "Movimentos Diagonais:"
      , Translate (-800) 20 $ Scale 1 1 $ Color black $ drawWord p "Q: Noroeste"
      , Translate (-800) (-30) $ Scale 1 1 $ Color black $ drawWord p "E: Nordeste"
      , Translate (-800) (-80) $ Scale 1 1 $ Color black $ drawWord p "Z: Sudoeste"
      , Translate (-800) (-130) $ Scale 1 1 $ Color black $ drawWord p "C: Sudeste"
      ]

    conteudoPagina 3 = Pictures
      [ Translate (-800) 300 $ Scale 1.5 1.5 $ Color (makeColor 0.2 0.4 0.8 1.0) $ drawWord p "MODO PVP - CONTROLES"
      , Translate (-800) 200 $ Scale 1 1 $ Color black $ drawWord p "1: Trocar minhoca"
      , Translate (-800) 160 $ Scale 1 1 $ Color (greyN 0.4) $ drawWord p "  (Cicla para a proxima minhoca viva)"
      , Translate (-800) 90 $ Scale 1 1 $ Color black $ drawWord p "2: Trocar arma"
      , Translate (-800) 50 $ Scale 1 1 $ Color (greyN 0.4) $ drawWord p "  Ciclo: Jetpack -> Escavadora -> Bazuca"
      , Translate (-800) 20 $ Scale 1 1 $ Color (greyN 0.4) $ drawWord p "         -> Mina -> Dinamite -> Nenhuma"
      , Translate (-800) (-50) $ Scale 1 1 $ Color black $ drawWord p "ESC: Voltar ao menu"
      ]

    conteudoPagina 4 = Pictures
      [ Translate (-800) 300 $ Scale 1.5 1.5 $ Color (makeColor 0.2 0.4 0.8 1.0) $ drawWord p "MAP CREATOR - GERAL"
      , Translate (-800) 200 $ Scale 1 1 $ Color (greyN 0.4) $ drawWord p "Navegacao:"
      , Translate (-800) 160 $ Scale 1 1 $ Color black $ drawWord p "1: Alternar modo"
      , Translate (-800) 130 $ Scale 1 1 $ Color (greyN 0.4) $ drawWord p "  (Blocos -> Objetos -> Minhocas)"
      , Translate (-800) 80 $ Scale 1 1 $ Color black $ drawWord p "Setas Cima/Baixo: Navegar opcoes"
      , Translate (-800) 40 $ Scale 1 1 $ Color black $ drawWord p "Setas Esq/Dir: Navegar subopcoes"
      , Translate (-800) 0 $ Scale 1 1 $ Color black $ drawWord p "ESC: Voltar ao menu"
      , Translate (-800) (-70) $ Scale 1 1 $ Color (greyN 0.4) $ drawWord p "Mouse:"
      , Translate (-800) (-110) $ Scale 1 1 $ Color black $ drawWord p "Clique Esquerdo: Adicionar elemento"
      , Translate (-800) (-150) $ Scale 1 1 $ Color black $ drawWord p "Clique Direito: Remover elemento"
      , Translate (-800) (-220) $ Scale 0.7 0.7 $ Color (greyN 0.4) $ drawWord p "(Remove apenas do modo selecionado)"
      ]

    conteudoPagina 5 = Pictures
      [ Translate (-800) 350 $ Scale 1.5 1.5 $ Color (makeColor 0.2 0.4 0.8 1.0) $ drawWord p "MAP CREATOR - EDICAO"
      , Translate (-800) 270 $ Scale 1 1 $ Color (greyN 0.4) $ drawWord p "Edicao do Mapa:"
      , Translate (-800) 230 $ Scale 1 1 $ Color black $ drawWord p "K: Adicionar linha abaixo"
      , Translate (-800) 195 $ Scale 1 1 $ Color black $ drawWord p "L: Adicionar coluna a direita"
      , Translate (-800) 160 $ Scale 1 1 $ Color black $ drawWord p "N: Remover ultima linha"
      , Translate (-800) 125 $ Scale 1 1 $ Color black $ drawWord p "M: Remover ultima coluna"
      , Translate (-800) 60 $ Scale 1 1 $ Color (greyN 0.4) $ drawWord p "Edicao de Minhocas:"
      , Translate (-800) 20 $ Scale 1 1 $ Color black $ drawWord p "Enter: Ativar/desativar modo edicao"
      , Translate (-800) (-15) $ Scale 1 1 $ Color black $ drawWord p "0-9: Digitar valores"
      , Translate (-800) (-50) $ Scale 1 1 $ Color black $ drawWord p "Backspace/Delete: Apagar digito"
      , Translate (-800) (-85) $ Scale 1 1 $ Color black $ drawWord p "Seta Esq/Dir: Navegar atributos"
      , Translate (-800) (-150) $ Scale 1 1 $ Color (greyN 0.4) $ drawWord p "Salvar/Carregar:"
      , Translate (-800) (-185) $ Scale 1 1 $ Color black $ drawWord p "E: Exportar estado"
      , Translate (-800) (-220) $ Scale 0.7 0.7 $ Color (greyN 0.4) $ drawWord p "  (Salva em 'estado.txt')"
      ]

    conteudoPagina _ = Blank

-- * Funções de Seleção e Game Over

{-| Desenha o seletor de níveis com lista de mapas disponíveis.

Funcionamento:

* Apresenta uma lista vertical de níveis disponíveis
* Mostra informações sobre cada nível (dimensões do mapa)
* Destaca o nível selecionado
* Cada nível é apresentado numa caixa individual com preview

Parâmetros:

* @p@: lista de recursos gráficos ('Picture')
* @selected@: índice do nível selecionado
* @estadosImportados@: lista de estados DLC com mapas importados

==__Exemplo de Utilização:__

>>> drawLvlSelector recursos 0 [estado1, estado2, estado3]
Picture (lista de níveis com primeiro selecionado)
-}

drawLvlSelector :: [Picture] -> Int -> [EstadoDLC] -> Picture
drawLvlSelector p selected estadosImportados = Pictures
 ([p !! 88] ++ (zipWith drawNivel [0..] estadosImportados))

  where
    drawNivel :: Int -> EstadoDLC -> Picture
    drawNivel idx estado = Translate 0 y $ Pictures
      [ Scale 1 1 $ (if idx == selected then p !! 89 else p !! 90)
      , Translate (x - largura/4) 0 $ Scale 0.7 0.7 $ drawWord p ("Level " ++ show (idx + 1))
      , Translate (x + largura/4 - 100) 0 $ Scale 0.6 0.6 $ Color black $ drawWord p infoMapa
      , Translate (x - 20) (-140) $ Scale 0.9 0.9 $ p !! 91
      ]
      where
        mapa = mapaEstadoDLC estado
        infoMapa = show (length mapa) ++ "x" ++ show (if null mapa then 0 else length (head mapa))
        largura = 1000
        altura  = 50
        espaco  = 90
        x = 20

        y = 300 - fromIntegral idx * (altura + espaco)


{-| Desenha o ecrã de fim de jogo com a equipa vencedora.

Funcionamento:

* Apresenta mensagem de vitória para a equipa vencedora
* Mostra fundo personalizado conforme a equipa (vermelha ou azul)
* Inclui instruções para retornar ao menu principal
* Usa cores e sprites temáticos para cada equipa

Parâmetros:

* @p@: lista de recursos gráficos ('Picture')
* @equipa@: equipa vencedora ('Red' ou 'Blue')

==__Exemplo de Utilização:__

>>> drawGameOver recursos Red
Picture (ecrã de vitória da equipa vermelha)

>>> drawGameOver recursos Blue
Picture (ecrã de vitória da equipa azul)
-}
drawGameOver :: [Picture] -> Team -> Picture
drawGameOver p equipa =
    Pictures
        [ bg
        , Pictures
            [ Translate (70) (-100) $ Scale 0.6 0.6 $ p !! 90
            , Translate (-200) (-100) $ Scale 0.4 0.4 $ Color (greyN 0.7) $
                drawWord p "Pressiona ESC para retornar ao menu"
            ]
        ]
  where
    bg = case equipa of
      Red -> Pictures
        [ Translate 0 0 $ Scale 1 1 $ p !! 115
        , Translate (80) 200 $ p !! 20
        , Translate (-110) 250 $ Scale 0.7 0.7 $ drawWord p "Equipa Vermelha"
        , Translate (-50) 200 $ Scale 1.5 1.5 $ Color black $ drawWord p "Ganha!"
        ]
      Blue -> Pictures
        [ Translate 0 0 $ Scale 1 1 $ p !! 116
        , Translate (80) 200 $ p !! 20
        , Translate (-70) 250 $ Scale 0.7 0.7 $ drawWord p "Equipa Azul"
        , Translate (-50) 200 $ Scale 1.5 1.5 $ Color black $ drawWord p "Ganha!"
        ]


-- * Funções de Desenho do Editor de Mapas

{-| Desenha a interface do Map Creator Tool (MCT) completa.

Funcionamento:

* Apresenta o mapa sendo editado no centro
* Mostra barra lateral com opções de edição
* Permite seleção de blocos, objetos e minhocas
* Exibe informações sobre o elemento selecionado
* Suporta três modos: Blocos, Objetos e Minhocas

Modos de edição:

* Modo 0 (Blocos): Terra, Água, Pedra, Ar, Lava, Gelo
* Modo 1 (Objetos): Barril, Health Pack, Ammo Packs, Disparos
* Modo 2 (Minhocas): Posicionamento e configuração de minhocas

Parâmetros:

* @p@: lista de recursos gráficos
* @e@: estado DLC atual do mapa
* @blocoSelecionado@: índice do elemento selecionado
* @mode@: modo de edição atual (0-2)
* @secSel@: seleção secundária (usado para subopções)
* @thirdSel@: seleção terciária
* @editMode@: se está em modo de edição ativo
* @_@: parâmetro não utilizado
* @MinhocaDLC{...}@: configuração da minhoca sendo editada
* @DisparoDLC{...}@: configuração do disparo sendo editado

==__Exemplo de Utilização:__

>>> drawMCT recursos estado 0 0 0 0 False Nothing minhocaDefault disparoDefault
Picture (editor com Terra selecionada no modo Blocos)
-}
drawMCT :: [Picture] -> EstadoDLC -> Int -> Int -> Int -> Int -> Bool -> Maybe Int -> MinhocaDLC -> ObjetoDLC -> Picture
drawMCT p e blocoSelecionado mode secSel thirdSel editMode _ (MinhocaDLC _ _ jet esc baz mina dina flame burn equipa _) (DisparoDLC _ dir _ tempoR dono) = Pictures
  [ p !! 88
  , Translate (-50) 50 $ p !! 97
  , sidebar, world]
  where
    mapa = mapaEstadoDLC e
    objs = objetosEstadoDLC e
    ms = minhocasEstadoDLC e

    infoMapa = show (length mapa) ++ "x" ++ show (if null mapa then 0 else length (head mapa))

    blocos = [(0, "Terra", Scale 0.66 0.66 $ p !! 0), (1, "Agua", p !! 1), (2, "Pedra", p !! 2), (3, "Ar", p !! 7), (4, "Lava", p !! 11), (5, "Gelo", p !! 120) ]

    staticObjects = [(0, "Barril", p !! 5), (1, "Health Pack", p !! 12)]
    ammoPacks = [(2 :: Int, "Jetpack", p !! 15), (2, "Escavadora", p !! 16), (2, "Bazuca", p !! 17), (2, "Mina", p !! 18), (2, "Dinamite", p !! 19)]
    disparos = [(3 :: Int, "Bazuca", p !! 6), (3, "Mina", p !! 9), (3, "Dinamite", p !! 8), (3, "FireBall", p !! 69)]

    personagens = [(0, "Pato", selectedSprite)]

    selectedSprite :: Picture
    selectedSprite =
       case equipa of
                Just Blue -> p !! 79
                Just Red -> p !! 78
                _ -> p !! 3




    drawBloco y (idx, nome, pic) = Pictures
      [ if idx == blocoSelecionado
          then Translate (-750) y $ Scale 5 5 $ p !! 126
          else Blank
      , Translate (-880) y $ Scale 0.6 0.6 $ drawWord p (show (idx + 1))
      , Translate (-850) (y + 5) $ Scale 0.6 0.6 $ Color black $ drawWord p nome
      , Translate (-700) y $ Scale 2 2 pic
      ]

    drawObjeto y (idx, nome, pic) = Pictures
      [ if idx == blocoSelecionado
          then Translate (-750) y $ Scale 5 5 $ p !! 126
          else Blank
      , Translate (-880) y $ Scale 0.6 0.6 $ Color black $ drawWord p (show (idx + 1))
      , Translate (-850) (y + 5) $ Scale 0.6 0.6 $ Color black $ drawWord p nome
      , Translate (-700) y $ Scale 2 2 pic
      ]

    drawAmmoPackSelector y = Pictures
      [ if blocoSelecionado == 2
          then Translate (-750) y $ Scale 5 5 $ p !! 126
          else Blank
      , Translate (-880) y $ Scale 0.6 0.6 $ Color black $ drawWord p (show (selectedIdx + 1))
      , Translate (-850) (y + 5) $ Scale 0.6 0.6 $ Color black $ drawWord p "Ammo Pack"
      , Translate (-600) y $ Scale 2 2 selectedPic
      , Translate (-850) (y - 30) $ Scale 0.5 0.5 $ Color (greyN 0.5) $ drawWord p "<"
      , Translate (-650) (y - 30) $ Scale 0.5 0.5 $ Color (greyN 0.5) $ drawWord p ">"
      , Translate (-825) (y - 30) $ Scale 0.5 0.5 $ Color (greyN 0.5) $ drawWord p selectedName

      ]
      where
        (selectedIdx, selectedName, selectedPic) = if blocoSelecionado == 2 then ammoPacks !! secSel else head ammoPacks


    drawDisparoSelector y = Pictures
      [ if blocoSelecionado == 3
          then  if editMode then Translate (-750) y $ Scale 5 5 $ p !! 129 else Translate (-750) y $ Scale 5 5 $ p !! 126
          else Blank
      , Translate (-880) y $ Scale 0.6 0.6 $ Color black $ drawWord p (show (selectedIdx + 1))
      , Translate (-850) (y + 5) $ Scale 0.6 0.6 $ Color black $ drawWord p "Disparos"
      , Translate (-600) y $ Scale 2 2 selectedPic
      , Translate (-850) (y - 30) $ Scale 0.5 0.5 $ Color (greyN 0.5) $ drawWord p "<"
      , Translate (-650) (y - 30) $ Scale 0.5 0.5 $ Color (greyN 0.5) $ drawWord p ">"
      , Translate (-825) (y - 30) $ Scale 0.5 0.5 $ Color (greyN 0.5) $ drawWord p selectedName
      , (if blocoSelecionado == 3  then Pictures
          [
            if thirdSel == 0 && editMode 
              then Color editColor $
                  Translate (-940) (y - 60) $ Scale 2 2 $ p !! 126
              else Blank
          , Translate (-940) (y - 60) $
              Scale 0.6 0.6 $
                Color black $
                  drawWord p ("Dir:" ++ show dir)
          ] else Blank)
      , if blocoSelecionado == 3 then Pictures
          [
            if thirdSel == 1 && editMode 
              then Color editColor $
                  Translate (-940) (y  - 120) $ Scale 2 2 $ p !! 126
              else Blank
          , Translate (-940) (y - 120) $
              Scale 0.6 0.6 $
                Color black $
                  drawWord p ("Tempo:" ++ show tempoR)
          ] else Blank
      , if blocoSelecionado == 3 then Pictures
          [
            if thirdSel == 2 && editMode 
              then Color editColor $
                  Translate (-940) (y - 180) $ Scale 2 2 $ p !! 126
              else Blank
          , Translate (-940) (y - 180) $
              Scale 0.6 0.6 $
                Color black $
                  drawWord p ("Dono:" ++ show dono)
          ] else Blank

      ]
      where
        (selectedIdx, selectedName, selectedPic) = if blocoSelecionado == 3 then disparos !! secSel else head disparos

    weaponOffset :: Float
    weaponOffset = 80

    weaponTextOffset :: Float
    weaponTextOffset = 150




    drawPersonagens y (idx, nome, pic) = Pictures
      [ if idx == blocoSelecionado
          then Translate (-750) y $ Scale 5 5 $ p !! 126
          else Blank
      , Translate (-880) y $ Scale 0.6 0.6 $ Color black $ drawWord p (show (idx + 1))
      , Translate (-850) (y + 5) $ Scale 0.6 0.6 $ Color black $ drawWord p nome
      , Translate (-700) y $ Scale 2 2 $ pic
      , Translate (-900) (y - weaponOffset) $ Scale 1.5 1.5 $ p !! 113
      , Translate (-830) (y - weaponOffset) $ Scale 1.5 1.5 $ p !! 114
      , Translate (-760) (y - weaponOffset) $ Scale 1.5 1.5 $ p !! 6
      , Translate (-690) (y - weaponOffset) $ Scale 1.5 1.5 $ p !! 9
      , Translate (-620) (y - weaponOffset) $ Scale 1.5 1.5 $ p !! 8
      , Translate (-820) (y - 3*weaponOffset) $ Scale 1.5 1.5 $ p !! 130

      , Translate (-900) (y -  3*weaponOffset) $ Scale 1.5 1.5 $ p !! 69
      

      , Pictures
          [
            if thirdSel == 0
              then Color editColor $
                  Translate (-910) (y - weaponTextOffset) $ Scale 0.5 2 $ p !! 126
              else Blank
          , Translate (-920) (y - weaponTextOffset) $
              Scale 0.4 0.4 $
                Color black $
                  drawWord p (show jet)
          ]

      , Pictures
          [
            if thirdSel == 1
              then Color editColor $
                  Translate (-840) (y - weaponTextOffset) $ Scale 0.5 2 $ p !! 126
              else Blank
          , Translate (-830) (y - weaponTextOffset) $
              Scale 0.4 0.4 $
                Color black $
                  drawWord p (show esc)
          ]

      , Pictures
          [
            if thirdSel == 2
              then Color editColor $
                  Translate (-750) (y - weaponTextOffset) $ Scale 0.5 2 $ p !! 126
              else Blank
          , Translate (-760) (y - weaponTextOffset) $
              Scale 0.4 0.4 $
                Color black $
                  drawWord p (show baz)
          ]

      , Pictures
          [
            if thirdSel == 3
              then Color editColor $
                  Translate (-700) (y - weaponTextOffset) $ Scale 0.5 2 $ p !! 126
              else Blank
          , Translate (-690) (y - weaponTextOffset) $
              Scale 0.4 0.4 $
                Color black $
                  drawWord p (show mina)
          ]

      , Pictures
          [
            if thirdSel == 4
              then Color editColor $
                  Translate (-630) (y - weaponTextOffset) $ Scale 0.5 2 $ p !! 126
              else Blank
          , Translate (-620) (y - weaponTextOffset) $
              Scale 0.4 0.4 $
                Color black $
                  drawWord p (show dina)
          ]


      , Pictures
          [
            if thirdSel == 5
              then Color editColor $
                  Translate (-910) (y - weaponTextOffset - 140) $ Scale 0.5 2 $ p !! 126
              else Blank
          , Translate (-920) (y - weaponTextOffset - 140) $
              Scale 0.4 0.4 $
                Color black $
                  drawWord p (show flame)
          ]

      , Pictures
          [
            if thirdSel == 6
              then Color editColor $
                  Translate (-600) (y - weaponTextOffset - 140) $ Scale 0.5 2 $ p !! 126
              else Blank
          , Translate (-830) (y - weaponTextOffset - 140) $
              Scale 0.4 0.4 $
                Color black $
                  drawWord p (show burn)
          ]

      , Pictures
        [ if thirdSel == 7
            then Color editColor $
              Translate (-800) (y - weaponTextOffset - 240) $ Scale 0.5 2 $ p !! 126
            else Blank
        , Translate (-800) (y - weaponTextOffset - 240) $ Scale 0.6 0.6 $ drawWord p "Equipa:"

        ,Translate (-760) (y - weaponTextOffset - 320) $
            Scale 3 3 $
              case equipa of
                Just Red -> p !! 128
                Just Blue -> p !! 127
                Nothing -> Color (greyN 0.5) $ rectangleSolid 70 70
        ]



          ]



    sidebar = Pictures
      [ Translate (-750) 50 $ p !! 94
      , Translate (-900) 360 $ Scale 1 1 $ Color black $ drawWord p ("Mapa:" ++ infoMapa)
      , Color (greyN 0.7) $ Translate (-750) 320 $ rectangleSolid 280 2
      , Translate (-920) 280 $ Scale 0.6 0.6 $ Color (greyN 0.3) $ drawWord p (case mode of
        0 -> "Blocos:"
        1 -> "Objetos:"
        2 -> "Personagens:"
        
        
        )

      , case mode of
          0 -> Pictures $ zipWith drawBloco [210, 120, 30, -60, -150, -240] blocos
          1 -> Pictures $
                 zipWith drawObjeto [210, 120] staticObjects ++
                 [drawAmmoPackSelector 30] ++
                 [drawDisparoSelector (-60)]
          2 -> Pictures $ zipWith drawPersonagens [210] personagens



      ]

    linha = length mapa
    cols = if null mapa then 0 else length (head mapa)
    largura = fromIntegral cols * cellSize
    altura  = fromIntegral linha * cellSize
    sidebarWidth = 300
    usableWidth  = janelaLargura - sidebarWidth
    usableHeight = janelaAltura

    sx = if largura > 0 then usableWidth / largura else 1
    sy = if altura > 0 then usableHeight / altura else 1
    scaleFactor = min (min sx sy) 2.0

    editColor = if editMode then red else makeColor 0.3 0.6 1.0 0.3

    world =
      Translate (sidebarWidth / 2) 0 $
        Scale scaleFactor scaleFactor $
          Pictures
            [ drawMapaDLC p mapa
            , drawMinhocasStatic p ms mapa
            , drawObjetosDLC p objs mapa
            ]


{-

      , Translate (-900) (-330) $ Scale 0.6 0.6 $ Color (greyN 0.5) $ drawWord p "L - Adicionar linha"
      , Translate (-900) (-360) $ Scale 0.6 0.6 $ Color (greyN 0.5) $ drawWord p "C - Adicionar coluna"
      , Translate (-900) (-390) $ Scale 0.6 0.6 $ Color (greyN 0.5) $ drawWord p "M - Remover coluna"
      , Translate (-900) (-420) $ Scale 0.6 0.6 $ Color (greyN 0.5) $ drawWord p "N - Remover linha"
      , Translate (-900) (-450) $ Scale 0.6 0.6 $ Color (greyN 0.5) $ drawWord p "1 - Selecionar bloco"
      , Translate (-900) (-480) $ Scale 0.6 0.6 $ Color (greyN 0.5) $ drawWord p "LMB - Colocar bloco/objeto/personagem"
      , Translate (-900) (-510) $ Scale 0.6 0.6 $ Color (greyN 0.5) $ drawWord p "RMB - Remover (Modo Selecionado)"
      , Translate (-900) (-540) $ Scale 0.6 0.6 $ Color (greyN 0.5) $ drawWord p "E - Exportar estado"
      , Translate (-900) (-570) $ Scale 0.6 0.6 $ Color (greyN 0.5) $ drawWord p "< > - Navegar"
      , Translate (-900) (-600) $ Scale 0.6 0.6 $ Color (greyN 0.5) $ drawWord p "Enter - Editar valor"
      , Translate (-900) (-630) $ Scale 0.6 0.6 $ Color (greyN 0.5) $ drawWord p "Backspace/Delete - Eliminar valor"

      , Translate (-200) (-680) $ Scale 0.6 0.6 $ Color (greyN 0.5) $ drawWord p ("1sel (a) > " ++ show mode) -- ? debug
      , Translate (0) (-680) $ Scale 0.6 0.6 $ Color (greyN 0.5) $ drawWord p ("     -  2sel (l)> " ++ show secSel) -- ? debug
      , Translate (200) (-680) $ Scale 0.6 0.6 $ Color (greyN 0.5) $ drawWord p ("                  -  3sel (t)> " ++ show thirdSel) -- ? debug
      , Translate (-200) (-780) $ Scale 0.6 0.6 $ Color (greyN 0.5) $ drawWord p ("                  -  b" ++ show blocoSelecionado) -- ? debug
-}

cellSize :: Float
cellSize = 32

-- * Funções de Desenho de Jogo

{-| Desenha o estado de jogo no modo Bot Simulation.

Funcionamento:

* Renderiza o mapa completo do jogo
* Mostra todas as minhocas e objetos
* Apresenta barra lateral com informações do estado
* Destaca a minhoca ativa e sua última jogada
* Exibe estatísticas: minhocas vivas/mortas, objetos

Componentes visuais:

* Mundo do jogo: mapa, minhocas, objetos
* Sidebar: sprite da minhoca selecionada, estatísticas
* Informações do mapa: dimensões, número de elementos

Parâmetros:

* @p@: lista de recursos gráficos
* @est@: estado atual do jogo
* @numMinhoca@: índice da minhoca ativa (opcional)
* @jogada@: última jogada executada (opcional)

==__Exemplo de Utilização:__

>>> drawGame recursos estado (Just 0) (Just (Move Norte))
Picture (jogo com minhoca 0 após mover para Norte)

>>> drawGame recursos estado Nothing Nothing
Picture (jogo sem jogada destacada)
-}
drawGame :: [Picture] -> Estado -> Maybe NumMinhoca -> Maybe Jogada -> Picture
drawGame p est numMinhoca jogada = Pictures [p !! 88, sidebar, world]
  where
    mapa = mapaEstado est
    infoMapa = "mapa: " ++ show (length mapa) ++ "x" ++ show (if null mapa then 0 else length (head mapa))
    objs = objetosEstado est
    ms = minhocasEstado est

    minhocasVivas = length (getMinhocasValidas ms)
    totalMinhocas = length ms
    totalObjetos = length objs

    sidebar = Pictures
      ( [ Translate (-50) 0 $ Scale 1.05 1 $ p !! 97
        , Translate (-750) 0 $ Scale 1.1 1.1 $ selectedSprite
        , Color (greyN 0.9) $ Translate (-750) 0 $ p !! 94
        , Translate (-950) 300 $ Scale 0.6 0.6 $ Color black $ drawWord p infoMapa
        , Translate (-950) 260 $ Scale 0.6 0.6 $ Color (dark green) $ drawWord p ("Minhocas vivas: " ++ show minhocasVivas)
        , Translate (-950) 220 $ Scale 0.6 0.6 $ Color (dark red) $ drawWord p ("Minhocas mortas: " ++ show (totalMinhocas - minhocasVivas))
        , Translate (-950) 180 $ Scale 0.6 0.6 $ Color black $ drawWord p ("Total minhocas: " ++ show totalMinhocas)
        , Translate (-950) 140 $ Scale 0.6 0.6 $ Color (dark red) $ drawWord p ("Objetos: " ++ show totalObjetos)
        ] ++ minhocasSidebar)

    selectedSprite :: Picture
    selectedSprite = case numMinhoca of
      Just idx | idx >= 0 && idx < length ms ->
        let m = ms !! idx in
        case posicaoMinhoca m of
          Just s -> getSpriteParaAcao m jogada p True mapa s
          Nothing -> p !! 3
      _ -> p !! 3

    minhocasSidebar :: [Picture]
    minhocasSidebar = case numMinhoca of
      Just idx | idx >= 0 && idx < length ms -> [drawMinhocaBar idx (ms !! idx)]
      _ -> []

    drawMinhocaBar :: Int -> Minhoca -> Picture
    drawMinhocaBar i m = Pictures
      [ Translate (-900) 40 $ Scale 0.6 0.6 $ drawWord p ("Player " ++ show i)
      , Translate (-900) 0 $ Scale 2 2 $ (if vidaMinhoca m == Morta then p !! 4 else p !! 3)
      , Translate (-930) (-80) $ Scale 1.5 1.5 $ p !! 113 -- jetpack
      , Translate (-840) (-80) $ Scale 1.5 1.5 $ p !! 114 -- escavadora
      , Translate (-750) (-80) $ Scale 1.5 1.5 $ p !! 6   -- bazuca
      , Translate (-660) (-80) $ Scale 1.5 1.5 $ p !! 9   -- mina
      , Translate (-570) (-80) $ Scale 1.5 1.5 $ p !! 8   -- dinamite
      , Translate (-930) (-130) $ Scale 0.6 0.6 $ drawWord p (show (jetpackMinhoca m))
      , Translate (-840) (-130) $ Scale 0.6 0.6 $ drawWord p (show (escavadoraMinhoca m))
      , Translate (-750) (-130) $ Scale 0.6 0.6 $ drawWord p (show (bazucaMinhoca m))
      , Translate (-660) (-130) $ Scale 0.6 0.6 $ drawWord p (show (minaMinhoca m))
      , Translate (-570) (-130) $ Scale 0.6 0.6 $ drawWord p (show (dinamiteMinhoca m))
      ]

    linha = length mapa
    cols = if null mapa then 0 else length (head mapa)

    larguraMapa = fromIntegral cols * cellSize
    alturaMapa  = fromIntegral linha * cellSize

    sidebarWidth = 200
    maxWorldWidth  = janelaLargura - sidebarWidth
    maxWorldHeight = janelaAltura - 400

    scaleX = if larguraMapa > 0 then maxWorldWidth / larguraMapa else 1
    scaleY = if alturaMapa > 0 then maxWorldHeight / alturaMapa else 1

    scaleFactor = min (min scaleX scaleY) 2.0


    world =
      Translate (sidebarWidth / 2 + 100) 0 $
        Scale scaleFactor scaleFactor $
          Pictures
            [ drawMapa p mapa
            , drawObjetos p objs mapa
            , drawMinhocas p ms mapa numMinhoca jogada
            ]

{-| Desenha o estado de jogo no modo Player vs Player (PVP).

Funcionamento:

* Renderiza o mapa com todas as minhocas e objetos (versão DLC)
* Mostra barra lateral com informações das equipas
* Destaca a minhoca selecionada pelo jogador
* Exibe estatísticas de ambas as equipas
* Apresenta inventário de armas da minhoca ativa

Diferenças do Bot Simulation:

* Usa tipos DLC ('EstadoDLC', 'MinhocaDLC', etc.)
* Suporta equipas Red e Blue
* Mostra sprite personalizado por equipa
* Inclui sistema de seleção de minhoca pelo jogador

Parâmetros:

* @p@: lista de recursos gráficos
* @est@: estado DLC atual do jogo
* @jogada@: última jogada executada pelo jogador

==__Exemplo de Utilização:__

>>> drawPvPGame recursos estadoDLC (Move Norte)
Picture (jogo PvP após movimento para Norte)

>>> drawPvPGame recursos estadoDLC (Dispara BazucaDLC Este)
Picture (jogo PvP após disparo de bazuca)
-}
drawPvPGame :: [Picture] -> EstadoDLC -> JogadaDLC -> Picture
drawPvPGame p est jogada =
  Pictures [p !! 88, p!!97, sidebar, world]
  where
    mapa = mapaEstadoDLC est
    objs = objetosEstadoDLC est
    ms   = minhocasEstadoDLC est

    infoMapa =
      "mapa: " ++ show (length mapa) ++ "x" ++
      show (if null mapa then 0 else length (head mapa))

    minhocasVivas = length (getMinhocasValidasDLC ms)
    totalMinhocas = length ms
    totalObjetos  = length objs

    selectedSprite :: Picture
    selectedSprite =
      let idx = minhocaSelecionada est
      in if idx >= 0 && idx < length ms
         then case equipaMinhoca (ms !! idx) of
                Just Blue -> p !! 95
                Just Red -> p !! 96
                _ -> p !! 95
         else p !! 95

    sidebar =
      Pictures
        ( [ Translate (-750) 0 $ Scale 1 1 selectedSprite
          , Translate (-920) 300 $ Scale 0.9 0.9 $ Color black $ drawWord p infoMapa
          , Translate (-920) 260 $ Scale 0.6 0.6 $ Color (dark green)
              $ drawWord p ("Minhocas vivas: " ++ show minhocasVivas)
          , Translate (-920) 220 $ Scale 0.6 0.6 $ Color (dark red)
              $ drawWord p ("Minhocas mortas: " ++ show (totalMinhocas - minhocasVivas))
          , Translate (-920) 180 $ Scale 0.6 0.6 $ Color black
              $ drawWord p ("Total minhocas: " ++ show totalMinhocas)
          , Translate (-920) 140 $ Scale 0.6 0.6 $ Color black
              $ drawWord p ("Objetos: " ++ show totalObjetos)
          , Translate (-770) 100 $
            Color black $
            rectangleSolid 560 2
          ]
          ++ minhocasSidebar
        )


    weaponOffset :: Float
    weaponOffset = 120

    minhocasSidebar :: [Picture]
    minhocasSidebar =
      let minhocaAtualIndex = minhocaSelecionada est
      in if minhocaAtualIndex >= 0 && minhocaAtualIndex < length ms
         then [drawMinhocaBar minhocaAtualIndex (ms !! minhocaAtualIndex)]
         else []

    drawMinhocaBar :: Int -> MinhocaDLC -> Picture
    drawMinhocaBar i m =
      Pictures
          [  -- Nome / índice
            Translate (-800) y $
            Scale 0.6 0.6 $
            Color (if armaSelecionada est == Nothing then dark red else black) $
                  drawWord p ("Player " ++ show i)
          -- Ícone vida
          , Translate (-900) (y - 20) $ Scale 2.5 2.5 $ (case equipaMinhoca m of
            Just Blue -> p !! 79
            Just Red -> p !! 78
            _ -> p !! 3)
          , Translate (-800) (y - 30) $
            Scale 0.6 0.6 $
            drawWord p ("HP: " ++ extrairVida (show (vidaMinhocaDLC m)))
          -- Equipa
          , case equipaMinhoca m of
              Just Red -> Translate (-600) (y - 30) $ Color red $ rectangleSolid 20 20
              Just Blue -> Translate (-600) (y - 30) $ Color blue $ rectangleSolid 20 20
              Nothing -> Blank
          -- Posição
          , Translate (-800) (y - 60) $
            Scale 0.6 0.6 $
            drawWord p ("POS: " ++ extrairPosicao (show (posicaoMinhocaDLC m)))
          -- Armas / itens (imagens)
          , if armaSelecionada est == Just JetpackDLC
              then Translate (-930) (y - weaponOffset) $ Scale 1.8 1.8 $ p !! 117
              else Blank
          , Translate (-930) (y - weaponOffset) $ Scale 1.5 1.5 $ p !! 113

          , if armaSelecionada est == Just EscavadoraDLC
              then Translate (-840) (y - weaponOffset) $ Scale 1.8 1.8 $ p !! 117
              else Blank
          , Translate (-840) (y - weaponOffset) $ Scale 1.5 1.5 $ p !! 114

          , if armaSelecionada est == Just BazucaDLC
              then Translate (-750) (y - weaponOffset) $ Scale 1.8 1.8 $ p !! 117
              else Blank
          , Translate (-750) (y - weaponOffset) $ Scale 1.5 1.5 $ p !! 6

          , if armaSelecionada est == Just MinaDLC
              then Translate (-660) (y - weaponOffset) $ Scale 1.8 1.8 $ p !! 117
              else Blank
          , Translate (-660) (y - weaponOffset) $ Scale 1.5 1.5 $ p !! 9

          , if armaSelecionada est == Just DinamiteDLC
              then Translate (-570) (y - weaponOffset) $ Scale 1.8 1.8 $ p !! 117
              else Blank
          , Translate (-570) (y - weaponOffset) $ Scale 1.5 1.5 $ p !! 8

          , if armaSelecionada est == Just FlameTrower
              then Translate (-930) (y - 2 * weaponOffset) $ Scale 1.8 1.8 $ p !! 117
              else Blank
          , Translate (-930) (y - 2 * weaponOffset) $ Scale 1.5 1.5 $ p !! 69

          -- Barras divisórias verticais
          , Translate (-885) (y - weaponOffset - 10) $
            Color (greyN 0.3) $
            rectangleSolid 2 60
          , Translate (-795) (y - weaponOffset - 10) $
            Color (greyN 0.3) $
            rectangleSolid 2 60
          , Translate (-705) (y - weaponOffset - 10) $
            Color (greyN 0.3) $
            rectangleSolid 2 60
          , Translate (-615) (y - weaponOffset - 10) $
            Color (greyN 0.3) $
            rectangleSolid 2 60
          -- Textos das armas (abaixo das imagens)
          , Translate (-938) (y - weaponOffset - 60) $
            Scale 0.6 0.6 $
            Color (if armaSelecionada est == Just JetpackDLC then dark red else black) $
                  drawWord p (show (jetpackMinhocaDLC m) ++ " ")
          , Translate (-848) (y - weaponOffset - 60) $
            Scale 0.6 0.6 $
            Color (if armaSelecionada est == Just EscavadoraDLC then dark red else black) $
                  drawWord p (show (escavadoraMinhocaDLC m) ++ " ")
          , Translate (-758) (y - weaponOffset - 60) $
            Scale 0.6 0.6 $
            Color (if armaSelecionada est == Just BazucaDLC then dark red else black) $
                  drawWord p (show (bazucaMinhocaDLC m) ++ " ")
          , Translate (-668) (y - weaponOffset - 60) $
            Scale 0.6 0.6 $
            Color (if armaSelecionada est == Just MinaDLC then dark red else black) $
                  drawWord p (show (minaMinhocaDLC m) ++ " ")
          , Translate (-578) (y - weaponOffset - 60) $
            Scale 0.6 0.6 $
            Color (if armaSelecionada est == Just DinamiteDLC then dark red else black) $
                  drawWord p (show (dinamiteMinhocaDLC m) ++ " ")
          , Translate (-938) (y - weaponOffset - 180) $
            Scale 0.6 0.6 $
            Color (if armaSelecionada est == Just FlameTrower then dark red else black) $
                  drawWord p (show (flameMinhocaDLC m) ++ " ")
          ]
        where
          y = 55

    linha = length mapa
    cols = if null mapa then 0 else length (head mapa)

    larguraMapa = fromIntegral cols * cellSize
    alturaMapa  = fromIntegral linha * cellSize

    sidebarWidth = 1000
    maxWorldWidth  = janelaLargura - sidebarWidth
    maxWorldHeight = janelaAltura - 400

    scaleX = if larguraMapa > 0 then maxWorldWidth / larguraMapa else 1
    scaleY = if alturaMapa > 0 then maxWorldHeight / alturaMapa else 1

    scaleFactor = min (min scaleX scaleY) 2.0


    world =
      Translate 100 0 $
        Scale scaleFactor scaleFactor $
          Pictures
            [ drawMapaDLC p mapa
            , drawMinhocasDLC p ms mapa (Just 0) jogada est
            , drawObjetosDLC p objs mapa
            , drawDanosDLC p est
            ]


-- * Funções de Conversão de Coordenadas

{-| Converte coordenadas de matriz do mapa para coordenadas de ecrã.

Funcionamento:

* Calcula dimensões totais do mapa
* Centra o mapa no ecrã
* Converte índices (linha, coluna) para coordenadas (x, y) em pixels
* Sistema de coordenadas: origem no centro, Y positivo para cima

Cálculos:

* Origem no centro do ecrã
* Cada célula tem tamanho 'cellSize' (32 pixels)
* Linha 0 no topo, cresce para baixo
* Coluna 0 à esquerda, cresce para a direita

Parâmetros:

* @mapa@: mapa para obter dimensões
* @(r,c)@: posição na matriz (linha, coluna)

==__Exemplo de Utilização:__

>>> converteMapa [[Ar,Ar],[Terra,Terra]] (0,0)
(-16.0, 16.0)
-}
converteMapa :: Mapa -> Posicao -> (Float, Float)
converteMapa mapa (r,c) = (x,y)
  where
    linha = length mapa
    cols = if null mapa then 0 else length (head mapa)
    largura = fromIntegral cols * cellSize
    altura = fromIntegral linha * cellSize
    left = - largura / 2 + cellSize / 2
    top = altura / 2 - cellSize / 2
    x = left + fromIntegral c * cellSize
    y = top - fromIntegral r * cellSize


{-| Conta quantos blocos de água existem diretamente acima de uma posição.

Funcionamento:

* Percorre a coluna verticalmente para cima
* Conta blocos consecutivos de 'Agua'
* Para quando encontra outro tipo de bloco
* Retorna 0 se já está no topo do mapa

Usado para determinar o sprite visual de água (níveis de profundidade).

Parâmetros:

* @mapa@: mapa do jogo
* @r@: linha (índice)
* @c@: coluna (índice)

==__Exemplo de Utilização:__

>>> contaAguasAcima [[Ar,Ar],[Agua,Agua],[Agua,Terra]] 2 0
1
-}
contaAguasAcima :: Mapa -> Int -> Int -> Int
contaAguasAcima mapa r c
  | r <= 0 = 0
  | otherwise = case mapa !! (r-1) !! c of
      Agua -> 1 + contaAguasAcima mapa (r-1) c
      _ -> 0

-- * Funções Auxiliares de Desenho

{-| Desenha o mapa do jogo completo.

Funcionamento:

* Percorre todas as linhas e colunas do mapa
* Desenha cada bloco com o sprite apropriado
* Aplica textura diferente para Terra com Ar acima
* Mostra diferentes níveis de profundidade para Água
* Adiciona contorno a cada célula do mapa

Texturas aplicadas:

* Ar: sprite transparente
* Terra: textura diferente se tiver Ar acima
* Água: 4 níveis visuais baseados na profundidade
* Pedra: textura sólida

Parâmetros:

* @p@: lista de recursos gráficos
* @mapa@: matriz representando o mapa do jogo

==__Exemplo de Utilização:__

>>> drawMapa recursos [[Ar,Ar],[Terra,Terra]]
Picture (mapa 2x2 renderizado)
-}
drawMapa :: [Picture] -> Mapa -> Picture
drawMapa p mapa = Pictures $ concatMap drawRow (zip [0..] mapa)
  where
    drawRow (r, row) = map (drawTile r) (zip [0..] row)
    drawTile r (c, t) = Translate x y $ Pictures [colorTile r c t, Color (greyN 0.6) $ rectangleWire cellSize cellSize]
      where
        (x,y) = converteMapa mapa (r,c)
        colorTile _ _ Ar = p !! 7
        colorTile linha col Agua = 
          let aguasAcima = contaAguasAcima mapa linha col
          in case aguasAcima of
               0 -> p !! 1
               1 -> p !! 121
               2 -> p !! 122
               _ -> p !! 123
        colorTile linha col Terra | linha > 0 && (mapa !! (linha-1) !! col) == Ar = Scale 0.66 0.66 $ p !! 0
                                 | otherwise = Scale 0.66 0.66 $ p !! 10
        colorTile _ _ Pedra = p !! 2

{-| Desenha todos os objetos presentes no estado do jogo.

Funcionamento:

* Itera sobre a lista de objetos
* Desenha cada tipo de objeto com o sprite apropriado
* Posiciona objetos nas coordenadas corretas do mapa
* Aplica rotação para disparos direcionais

Parâmetros:

* @p@: lista de recursos gráficos
* @objs@: lista de objetos a desenhar
* @mapa@: mapa para conversão de coordenadas

==__Exemplo de Utilização:__

>>> drawObjetos recursos [Barril (2,3) 3, Disparo (1,1) Norte Bazuca Nothing 0] mapa
Picture (barril e bazuca renderizados)
-}
drawObjetos :: [Picture] -> [Objeto] -> Mapa -> Picture
drawObjetos p objs mapa = Pictures $ map drawO objs
  where
    drawO o@(Disparo {}) = Translate x y $ case tipoDisparo o of
      Bazuca -> bazucaDir p (direcaoDisparo o)
      Mina -> p !! 9
      Dinamite -> p !! 8
      _ -> Color black $ circleSolid (cellSize * 0.4)
      where (x,y) = converteMapa mapa (Tarefa0_2025.posicaoObjeto o)

    drawO b@(Barril {}) = Translate x y $ p !! 5
      where (x,y) = converteMapa mapa (Tarefa0_2025.posicaoObjeto b)


{-| Desenha texto usando sprites de caracteres.

Funcionamento:

* Converte cada caractere para um sprite
* Posiciona sprites com espaçamento horizontal
* Suporta letras, números e alguns símbolos especiais
* Usa sistema de sprites bitmap para renderização

Espaçamento:

* Cada caractere é espaçado por 40 unidades
* Alinhamento da esquerda para a direita

Parâmetros:

* @p@: lista de recursos gráficos (incluindo sprites de caracteres)
* @str@: string a ser renderizada

==__Exemplo de Utilização:__

>>> drawWord recursos "WORMS"
Picture (palavra WORMS renderizada)

>>> drawWord recursos "Level 1"
Picture (texto com espaço renderizado)
-}
drawWord :: [Picture] -> String -> Picture
drawWord p str =
  Pictures $
    zipWith (\x pic -> Translate x 0 pic) xs (stringToPictures p str)
  where
    spacing = 40
    xs = [0, spacing ..]



{-| Converte uma string para uma lista de sprites de caracteres.

Funcionalidade:

* Mapeia cada caractere da string para um sprite
* Usa 'drawLetters' para conversão individual
* Suporta letras (a-z), números (0-9) e símbolos

Usado por:

* 'drawWord' para renderizar texto no jogo

Parâmetros:

* @p@: lista de recursos gráficos (sprites de caracteres)
* @str@: string a converter

==__Exemplo de Utilização:__

>>> stringToPictures recursos "abc"
[sprite_a, sprite_b, sprite_c]
-}
stringToPictures :: [Picture] -> String -> [Picture]
stringToPictures p str =
  map (drawLetters p) str


{-| Converte um caractere individual para o sprite correspondente.

Funcionalidade:

* Mapeia caractere para índice na lista de sprites
* Suporta letras minúsculas e maiúsculas (convertidas para minúsculas)
* Suporta números 0-9
* Suporta símbolos: espaço, '-', '(', ')', '/', ':', ',', '!', '?', '@', '<', '>'
* Retorna 'Blank' para caracteres não suportados

Parâmetros:

* @p@: lista de recursos gráficos
* @c@: caractere a converter

==__Exemplo de Utilização:__

>>> drawLetters recursos 'a'
sprite_a

>>> drawLetters recursos '5'
sprite_5
-}
drawLetters :: [Picture] -> Char -> Picture
drawLetters p c =
  let lc = toLower c in
  case lc of
    'a' -> p !! 23
    'b' -> p !! 24
    'c' -> p !! 25
    'd' -> p !! 26
    'e' -> p !! 27
    'f' -> p !! 28
    'g' -> p !! 29
    'h' -> p !! 30
    'i' -> p !! 31
    'j' -> p !! 32
    'k' -> p !! 33
    'l' -> p !! 34
    'm' -> p !! 35
    'n' -> p !! 36
    'o' -> p !! 37
    'p' -> p !! 38
    'q' -> p !! 39
    'r' -> p !! 40
    's' -> p !! 41
    't' -> p !! 42
    'u' -> p !! 43
    'v' -> p !! 44
    'w' -> p !! 45
    'x' -> p !! 46
    'y' -> p !! 47
    'z' -> p !! 48
    ' '   -> p !! 49
    '0' -> p !! 50
    '1' -> p !! 51
    '2' -> p !! 52
    '3' -> p !! 53
    '4' -> p !! 54
    '5' -> p !! 55
    '6' -> p !! 56
    '7' -> p !! 57
    '8' -> p !! 58
    '9' -> p !! 59
    '-' -> p !! 60
    '(' -> p !! 61
    ')' -> p !! 62
    '/' -> p !! 63
    ':' -> p !! 64
    ',' -> p !! 65
    '!' -> p !! 66
    '?' -> p !! 67
    '@' -> p !! 68
    '<' -> p !! 70
    '>' -> p !! 71
    _ -> Blank




{-| Aplica rotação apropriada ao sprite de bazuca conforme a direção.

Funcionamento:

* Recebe a direção do disparo
* Rotaciona o sprite base da bazuca
* Suporta 8 direções (cardeais e diagonais)
* Retorna sprite rotacionado pronto para renderização

Parâmetros:

* @p@: lista de recursos gráficos
* @dir@: direção do disparo

==__Exemplo de Utilização:__

>>> bazucaDir recursos Norte
Picture (bazuca apontando para cima)
-}
bazucaDir :: [Picture] -> Direcao -> Picture
bazucaDir p dir = case dir of
    Este -> p !! 6
    Oeste -> Rotate 180 (p !! 6)
    Norte -> Rotate 270 (p !! 6)
    Sul -> Rotate 90 (p !! 6)
    Sudeste -> Rotate 45 (p !! 6)
    Sudoeste -> Rotate 135 (p !! 6)
    Nordeste -> Rotate 315 (p !! 6)
    Noroeste -> Rotate 225 (p !! 6)

{-| Aplica rotação apropriada ao sprite de fireball (lança-chamas) conforme a direção.

Funcionamento:

* Similar a 'bazucaDir' mas para sprite de fireball
* Rotaciona sprite conforme direção do disparo
* Suporta direções cardeais e diagonais
* Usado para arma FlameTrower no modo DLC

Parâmetros:

* @p@: lista de recursos gráficos
* @dir@: direção do disparo

==__Exemplo de Utilização:__

>>> fireballDir recursos Sul
Picture (fireball apontando para baixo)
-}
fireballDir :: [Picture] -> Direcao -> Picture
fireballDir p dir = case dir of
    Este -> p !! 69
    Oeste -> Rotate 180 (p !! 69)
    Norte -> Rotate 270 (p !! 69)
    Sul -> Rotate 90 (p !! 69)
    Sudeste -> Rotate 45 (p !! 69)
    Sudoeste -> Rotate 135 (p !! 69)
    Nordeste -> Rotate 315 (p !! 69)
    Noroeste -> Rotate 225 (p !! 69)

{-| Desenha todas as minhocas do jogo com sprites apropriados.

Funcionamento:

* Itera sobre a lista de minhocas
* Seleciona sprite baseado no estado da minhoca
* Aplica animação diferente para minhoca ativa
* Mostra sprite de morte para minhocas mortas
* Considera a última jogada executada

Sprites baseados em:

* Estado: viva ou morta
* Jogada: movimento ou disparo
* Direção: horizontal (Este/Oeste)
* Terreno: no chão ou no ar

Parâmetros:

* @p@: lista de recursos gráficos
* @ms@: lista de minhocas a desenhar
* @mapa@: mapa para verificação de terreno
* @numMinhoca@: índice da minhoca ativa (opcional)
* @jogada@: última jogada executada (opcional)

==__Exemplo de Utilização:__

>>> drawMinhocas recursos [minhoca1, minhoca2] mapa (Just 0) (Just (Move Este))
Picture (minhocas com primeira animada movendo para Este)
-}
drawMinhocas :: [Picture] -> [Minhoca] -> Mapa -> Maybe NumMinhoca -> Maybe Jogada -> Picture
drawMinhocas p ms mapa numMinhoca jogada = Pictures $ map drawM (zip [0..] ms)
  where
    drawM (i,m) = case posicaoMinhoca m of
      Nothing -> Blank
      Just s -> Translate x y sprite
        where
          (x,y) = converteMapa mapa s
          sprite = if vidaMinhoca m == Morta
            then p !! 4  -- Morto
            else getSpriteParaAcao m jogada p (Just i == numMinhoca) mapa s



{-| Determina o sprite apropriado para uma minhoca baseado na sua jogada.

Funcionamento:

* Avalia o tipo de jogada (movimento ou disparo)
* Considera se a minhoca está ativa
* Verifica se está no chão ou no ar
* Determina direção horizontal (Este/Oeste)
* Retorna sprite apropriado para a situação

Sprites diferenciados por:

* Jogada: None (idle), Move, Dispara
* Estado: no chão vs no ar
* Direção: Este vs Oeste
* Ativa: minhoca selecionada vs outras
* Arma: cada arma tem sprite específico

Parâmetros:

* @minhoca@: minhoca a renderizar
* @jogada@: última jogada executada (opcional)
* @p@: lista de recursos gráficos
* @isActiveMinhoca@: se é a minhoca ativa
* @mapa@: mapa para verificação de terreno
* @_@: posição (não utilizada)

==__Exemplo de Utilização:__

>>> getSpriteParaAcao minhoca (Just (Move Este)) recursos True mapa pos
sprite_minhoca_movimento_este
-}
getSpriteParaAcao :: Minhoca -> Maybe Jogada -> [Picture] -> Bool -> Mapa -> Posicao -> Picture
getSpriteParaAcao _ Nothing p _ _ _ = p !! 3  -- Idle

getSpriteParaAcao minhoca (Just (Labs2025.Move dir)) p isActiveMinhoca mapa _

  | eMinhocaViva minhoca =
      let pos = case posicaoMinhoca minhoca of Just a -> a
          estaNoChao = Tarefa2.estaNoSolo pos mapa
          direcaoHorizontal = getXWay dir
      in if isActiveMinhoca then
        case direcaoHorizontal of
            Este -> if estaNoChao then p !! 99 else p !! 98
            _ ->  if estaNoChao then p !! 3 else p !! 13
        else if estaNoChao then p !! 3 else p !! 13


getSpriteParaAcao minhoca (Just (Labs2025.Dispara arma direcaoDisparo)) p isActiveMinhoca mapa _
  = let
        pos = case posicaoMinhoca minhoca of Just a -> a
        estaNoChao = Tarefa2.estaNoSolo pos mapa
        direcaoHorizontal = getXWay direcaoDisparo
    in
      if isActiveMinhoca
        then
          case direcaoHorizontal of

          Este ->

            case arma of
              Bazuca -> p !! 99
              Jetpack ->  p !! 99
              Escavadora -> p !! 99
              Dinamite -> p !! 99
              Mina -> p !! 99

          Oeste ->

            case arma of
              Bazuca -> p !! 3
              Jetpack ->  p !! 3
              Escavadora -> p !! 3
              Dinamite -> p !! 3
              Mina -> p !! 3
      else if estaNoChao then p !! 3 else p !! 13






{-| Desenha minhocas estáticas (sem animação) no editor de mapas.

Funcionamento:

* Renderiza minhocas em modo estático
* Usa sprites padrão sem animação
* Diferencia por equipa (Red/Blue)
* Mostra sprite de morte para minhocas mortas
* Usado principalmente no Map Creator Tool

Parâmetros:

* @p@: lista de recursos gráficos
* @minhocas@: lista de minhocas DLC a desenhar
* @mapa@: mapa DLC para conversão de coordenadas

==__Exemplo de Utilização:__

>>> drawMinhocasStatic recursos [minhocaDLC1, minhocaDLC2] mapaDLC
Picture (minhocas em pose estática)
-}
drawMinhocasStatic :: [Picture] -> [MinhocaDLC] -> MapaDLC -> Picture
drawMinhocasStatic p minhocas mapa = Pictures $ map drawM minhocas
  where
    drawM m = case posicaoMinhocaDLC m of
      Nothing -> Blank
      Just pos ->
        let (x, y) = converteMapaDLC mapa pos
            sprite = case vidaMinhocaDLC m of
              MortaDLC -> p !! 4
              VivaDLC _ -> case equipaMinhoca m of
                Just Blue -> p !! 79
                Just Red -> p !! 78
                Nothing -> p !! 3

        in Translate x y $ Pictures [sprite]




-- * Funções de Desenho DLC

{-| Conta quantos blocos de água existem diretamente acima de uma posição no mapa DLC.

Funcionamento:

* Percorre a coluna verticalmente para cima
* Conta blocos consecutivos de 'AguaDLC'
* Para quando encontra outro tipo de bloco
* Retorna 0 se já está no topo

Usado para determinar níveis visuais de profundidade da água.

Parâmetros:

* @mapa@: mapa DLC
* @r@: linha (índice)
* @c@: coluna (índice)

==__Exemplo de Utilização:__

>>> contaAguasAcimaDLC [[ArDLC,ArDLC],[AguaDLC,AguaDLC],[AguaDLC,TerraDLC]] 2 0
1
-}
contaAguasAcimaDLC :: MapaDLC -> Int -> Int -> Int
contaAguasAcimaDLC mapa r c
  | r <= 0 = 0
  | otherwise = case mapa !! (r-1) !! c of
      AguaDLC -> 1 + contaAguasAcimaDLC mapa (r-1) c
      _ -> 0

{-| Desenha o mapa DLC completo com todos os tipos de terreno.

Funcionamento:

* Similar a 'drawMapa' mas para tipos DLC
* Suporta tipos adicionais: Lava, Gelo
* Aplica texturas apropriadas por terreno
* Usa sistema de profundidade para água
* Adiciona contorno em cada célula

Tipos de terreno DLC:

* ArDLC, TerraDLC, AguaDLC (com níveis)
* PedraDLC, Lava, Gelo

Parâmetros:

* @p@: lista de recursos gráficos
* @mapa@: mapa DLC a desenhar

==__Exemplo de Utilização:__

>>> drawMapaDLC recursos [[ArDLC,TerraDLC],[TerraDLC,PedraDLC]]
Picture (mapa DLC renderizado)
-}
drawMapaDLC :: [Picture] -> MapaDLC -> Picture
drawMapaDLC p mapa = Pictures $ concatMap drawRow (zip [0..] mapa)
  where
    drawRow (r, row) = map (drawTile r) (zip [0..] row)
    drawTile r (c, t) = Translate x y $ Pictures [colorTile r c t, Color (greyN 0.6) $ rectangleWire cellSize cellSize]
      where
        (x,y) = converteMapaDLC mapa (r,c)
        colorTile _ _ ArDLC = p !! 7
        colorTile linha col AguaDLC = 
          let aguasAcima = contaAguasAcimaDLC mapa linha col
          in case aguasAcima of
               0 -> p !! 1
               1 -> p !! 121
               2 -> p !! 122
               _ -> p !! 123
        colorTile linha col TerraDLC | linha > 0 && (mapa !! (linha-1) !! col) == ArDLC = Scale 0.66 0.66 $ p !! 0
                                   | otherwise = Scale 0.66 0.66 $ p !! 10
        colorTile _ _ PedraDLC = p !! 2
        colorTile _ _ Lava = p !! 11
        colorTile _ _ Gelo = p !! 120

{-| Desenha todos os objetos DLC presentes no estado.

Funcionamento:

* Similar a 'drawObjetos' mas para tipos DLC
* Suporta objetos adicionais: HealthPack, AmmoPack, FlameTrower
* Posiciona cada objeto nas coordenadas do mapa DLC
* Aplica rotação para disparos direcionais

Tipos de objetos DLC:

* DisparoDLC: BazucaDLC, MinaDLC, DinamiteDLC, FlameTrower, EscavadoraDLC, JetpackDLC
* BarrilDLC: barril explosivo
* HealthPack: recuperação de vida
* AmmoPack: munição de diferentes tipos

Parâmetros:

* @p@: lista de recursos gráficos
* @objs@: lista de objetos DLC
* @mapa@: mapa DLC para conversão de coordenadas

==__Exemplo de Utilização:__

>>> drawObjetosDLC recursos [BarrilDLC (1,1) 3, HealthPack (2,2)] mapaDLC
Picture (barril e health pack renderizados)
-}
drawObjetosDLC :: [Picture] -> [ObjetoDLC] -> MapaDLC -> Picture
drawObjetosDLC p objs mapa = Pictures $ map drawO objs
  where
    drawO o@(DisparoDLC {}) = Translate x y $ case tipoDisparoDLC o of
      BazucaDLC -> bazucaDir p (direcaoDisparoDLC o)
      MinaDLC -> p !! 9
      DinamiteDLC -> p !! 8
      FlameTrower -> fireballDir p (direcaoDisparoDLC o)
      _ -> Color black $ circleSolid (cellSize * 0.4)
      where (x,y) = converteMapaDLC mapa (DataDLC.posicaoObjeto o)

    drawO b@(BarrilDLC {}) = Translate x y $ p !! 5
      where (x,y) = converteMapaDLC mapa (DataDLC.posicaoObjeto b)

    drawO hp@(HealthPack {}) = Translate x y $ p !! 12
      where (x,y) = converteMapaDLC mapa (DataDLC.posicaoObjeto hp)

    drawO hp@(AmmoPack {}) =
      case ammoType hp of
          JetpackDLC -> Translate x y $ p !! 15
          EscavadoraDLC -> Translate x y $ p !! 16
          BazucaDLC -> Translate x y $ p !! 17
          MinaDLC -> Translate x y $ p !! 18
          DinamiteDLC -> Translate x y $ p !! 19

      where (x,y) = converteMapaDLC mapa (DataDLC.posicaoObjeto hp)

{-| Determina o sprite apropriado para uma minhoca DLC baseado na jogada e estado.

Funcionamento:

* Similar a 'getSpriteParaAcao' mas para versão DLC
* Considera equipa da minhoca (Red/Blue)
* Aplica sprites de queimadura se 'burningCounter' > 0
* Usa sprites específicos por arma e equipa
* Verifica se está no chão para animação apropriada

Parâmetros:

* @minhoca@: minhoca DLC a renderizar
* @jogada@: jogada DLC executada
* @p@: lista de recursos gráficos
* @e@: estado DLC completo (para verificações de terreno)

==__Exemplo de Utilização:__

>>> getSpriteParaAcaoDLC minhoca (Move Norte) recursos estado
sprite_minhoca_equipa_movimento
-}
getSpriteParaAcaoDLC :: MinhocaDLC -> JogadaDLC -> [Picture] -> EstadoDLC -> Picture
getSpriteParaAcaoDLC minhoca (DataDLC.Move _) p e
  | burningCounter minhoca > 0 =
      case ultimaDirecaoHorizontal minhoca of
        Oeste -> p !! 84
        Este  -> p !! 100

  | eMinhocaVivaDLC minhoca =
      let pos = case posicaoMinhocaDLC minhoca of Just a -> a
          estaNoChao = EfetuaJogada.estaNoSolo pos (mapaEstadoDLC e) (minhocasEstadoDLC e) (objetosEstadoDLC e)
          direcaoHorizontal = ultimaDirecaoHorizontal minhoca
      in case equipaMinhoca minhoca of

        Just Red ->
          case direcaoHorizontal of
            Este ->
              if estaNoChao then p !! 104 else p !! 103
            _ ->
              if estaNoChao then p !! 78 else p !! 77

        Just Blue ->
          case direcaoHorizontal of
            Este ->
              if estaNoChao then p !! 110 else p !! 109
            _ ->
              if estaNoChao then p !! 79 else p !! 76

        _ ->
          case direcaoHorizontal of
            Este -> p !! 98
            _ -> p !! 3

getSpriteParaAcaoDLC minhoca (DataDLC.Dispara arma _) p e
  | burningCounter minhoca > 0 =
      case ultimaDirecaoHorizontal minhoca of
        Oeste -> p !! 84
        Este  -> p !! 100

  | eMinhocaVivaDLC minhoca =
      let pos = case posicaoMinhocaDLC minhoca of Just a -> a
          estaNoChao = EfetuaJogada.estaNoSolo pos (mapaEstadoDLC e) (minhocasEstadoDLC e) (objetosEstadoDLC e)
          direcaoHorizontal = ultimaDirecaoHorizontal minhoca
      in case equipaMinhoca minhoca of

        Just Red -> case arma of
          JetpackDLC     ->
            case direcaoHorizontal of
              Este -> p !! 109
              _ -> p !! 86
          EscavadoraDLC  ->
            case direcaoHorizontal of
              Este -> p !! 107
              _ -> p !! 83
          BazucaDLC      ->
            case direcaoHorizontal of
              Este -> p !! 105
              _ -> p !! 72
          _ ->
            case direcaoHorizontal of
              Este -> if estaNoChao then p !! 101 else p !! 102
              _ -> if estaNoChao then p !! 78 else p !! 77

        Just Blue -> case arma of
          JetpackDLC     ->
            case direcaoHorizontal of
              Este -> p !! 110
              _ -> p !! 87
          EscavadoraDLC  ->
            case direcaoHorizontal of
              Este -> p !! 108
              _ -> p !! 82
          BazucaDLC      ->
            case direcaoHorizontal of
              Este -> p !! 106
              _ -> p !! 73
          _ ->
            case direcaoHorizontal of
              Este -> if estaNoChao then p !! 103 else p !! 104
              _ -> if estaNoChao then p !! 79 else p !! 76

        _ ->
          case direcaoHorizontal of
            Este -> p !! 98
            _ -> p !! 3

{-| Converte coordenadas de matriz do mapa DLC para coordenadas de ecrã.

Funcionamento:

* Idêntica a 'converteMapa' mas para mapas DLC
* Calcula dimensões totais do mapa
* Centra o mapa no ecrã
* Converte índices para coordenadas em pixels

Sistema de coordenadas:

* Origem no centro do ecrã
* Cada célula: 32 pixels ('cellSize')
* Y positivo para cima
* X positivo para a direita

Parâmetros:

* @mapa@: mapa DLC para obter dimensões
* @(r,c)@: posição na matriz (linha, coluna)

==__Exemplo de Utilização:__

>>> converteMapaDLC [[ArDLC,TerraDLC],[TerraDLC,PedraDLC]] (1,1)
(16.0, -16.0)
-}
converteMapaDLC :: MapaDLC -> Posicao -> (Float, Float)
converteMapaDLC mapa (r,c) = (x,y)
  where
    linha = length mapa
    cols = if null mapa then 0 else length (head mapa)
    largura = fromIntegral cols * cellSize
    altura = fromIntegral linha * cellSize
    left = - largura / 2 + cellSize / 2
    top = altura / 2 - cellSize / 2
    x = left + fromIntegral c * cellSize
    y = top - fromIntegral r * cellSize


{-| Desenha as minhocas DLC com sprites animados baseados na jogada.

Funcionamento:

* Similar a 'drawMinhocas' mas para versão DLC
* Suporta sprites por equipa (Red/Blue)
* Mostra indicador visual para minhoca selecionada
* Aplica efeitos visuais (ex: queimadura)
* Usa sprites específicos por arma e ação

Parâmetros:

* @p@: lista de recursos gráficos
* @ms@: lista de minhocas DLC
* @mapa@: mapa DLC
* @_@: parâmetro não utilizado
* @jogada@: última jogada executada
* @e@: estado DLC completo (para minhoca selecionada)

==__Exemplo de Utilização:__

>>> drawMinhocasDLC recursos minhocas mapaDLC Nothing jogada estado
Picture (minhocas DLC renderizadas com animações)
-}
drawMinhocasDLC :: [Picture] -> [MinhocaDLC] -> MapaDLC -> Maybe NumMinhoca -> JogadaDLC -> EstadoDLC -> Picture
drawMinhocasDLC p ms mapa _ jogada e = Pictures $ map drawM (zip [0..] ms)
  where
    drawM (i,m) = case posicaoMinhocaDLC m of
      Nothing -> Blank
      Just s -> Translate x y $ Pictures
        [ sprite
        , if (i == minhocaSelecionada e) && eMinhocaVivaDLC m then (case ultimaDirecaoHorizontal m of
                Oeste -> if burningCounter m > 0
                  then p !! 118
                  else if EfetuaJogada.estaNoSolo s mapa ms (objetosEstadoDLC e) then Scale 1 1 $ p !! 92 else Scale 1 1 $ p !! 93
                Este -> if burningCounter m > 0
                  then p !! 119
                  else if EfetuaJogada.estaNoSolo s mapa ms (objetosEstadoDLC e) then Scale 1 1 $ p !! 101 else Scale 1 1 $ p !! 102) else Blank
        ]
        where
          (x,y) = converteMapaDLC mapa s
          sprite = if vidaMinhocaDLC m == MortaDLC
            then p !! 4  -- Morto
            else getSpriteParaAcaoDLC m jogada p e


-- * Funções de Efeitos Visuais e Utilidades

{-| Desenha sprites de explosão nos locais onde houve dano.

Funcionamento:

* Percorre a lista de danos no estado
* Filtra apenas danos dentro dos limites do mapa
* Desenha sprite de explosão em cada posição
* Usado para feedback visual de ataques

Parâmetros:

* @p@: lista de recursos gráficos
* @e@: estado DLC contendo histórico de danos

==__Exemplo de Utilização:__

>>> drawDanosDLC recursos estadoComDanos
Picture (explosões nas posições danificadas)
-}
drawDanosDLC :: [Picture] -> EstadoDLC -> Picture
drawDanosDLC p e = Pictures $ map drawDano $ filter dentroDoMapa (concat $ danosEstado e)
  where
    mapa = mapaEstadoDLC e
    
    dentroDoMapa (pos, _) = ePosicaoMatrizValida pos mapa
    
    drawDano (pos, _) = 
      let (x, y) = converteMapaDLC mapa pos
      in Translate x y $ p !! 124


{-| Extrai apenas o valor numérico da vida de uma string.

Funcionalidade:

* Processa string representando 'VidaMinhocaDLC'
* Retorna apenas o valor numérico da vida
* Funciona tanto para "VivaDLC n" como "MortaDLC"
* Retorna "N/A" se formato inválido

Parâmetros:

* @str@: string representando a vida

==__Exemplo de Utilização:__

>>> extrairVida "VivaDLC 100"
"100"

>>> extrairVida "MortaDLC"
"N/A"
-}
extrairVida :: String -> String
extrairVida str = case words str of
    ["VivaDLC", hp] -> hp
    ["MortaDLC", hp] -> hp
    _ -> "N/A"

{-| Extrai a posição de uma string formatada.

Funcionalidade:

* Processa string representando 'Maybe Posicao'
* Retorna apenas as coordenadas
* Retorna "N/A" para 'Nothing' ou formato inválido

Parâmetros:

* @str@: string representando a posição

==__Exemplo de Utilização:__

>>> extrairPosicao "Just (1,2)"
"(1,2)"

>>> extrairPosicao "Nothing"
"N/A"
-}
extrairPosicao :: String -> String
extrairPosicao str = case words str of
    ["Just", pos] -> pos
    ["Nothing"] -> "N/A"
    _ -> "N/A"