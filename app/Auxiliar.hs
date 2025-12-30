module Auxiliar where


import DataDLC( Matriz, Dimensao,VidaMinhocaDLC(VivaDLC, MortaDLC), posicaoObjeto, tipoDisparoDLC,donoDisparoDLC, TerrenoDLC(ArDLC, PedraDLC, TerraDLC, Gelo, AguaDLC), objetosEstadoDLC,TipoArmaDLC(FlameTrower,JetpackDLC, EscavadoraDLC, BazucaDLC, MinaDLC, DinamiteDLC), EstadoDLC(mapaEstadoDLC, objetosEstadoDLC, minhocasEstadoDLC), MapaDLC, ObjetoDLC(BarrilDLC, DisparoDLC), MinhocaDLC(posicaoMinhocaDLC, vidaMinhocaDLC,jetpackMinhocaDLC, escavadoraMinhocaDLC, minaMinhocaDLC, dinamiteMinhocaDLC, bazucaMinhocaDLC, flameMinhocaDLC))
import Labs2025(NumMinhoca,Posicao, Direcao(Norte,Este,Oeste,Sul,Nordeste,Noroeste,Sudoeste,Sudeste))


-- | Recebe uma lista e filtra as minhocas válidas (com posição e vivas)
getMinhocasValidasDLC :: [MinhocaDLC] -> [MinhocaDLC]
getMinhocasValidasDLC [] = []
getMinhocasValidasDLC (h:t) =
  if posicaoMinhocaDLC h /= Nothing && eMinhocaVivaDLC h
    then h : getMinhocasValidasDLC t
    else getMinhocasValidasDLC t


-- | Retorna a quantidade de munições disponíveis de uma minhoca para uma dada arma.
encontraQuantidadeArmaMinhoca :: TipoArmaDLC -> MinhocaDLC -> Int
encontraQuantidadeArmaMinhoca tipo m = case tipo of
    JetpackDLC -> jetpackMinhocaDLC m
    EscavadoraDLC -> escavadoraMinhocaDLC m
    BazucaDLC -> bazucaMinhocaDLC m
    MinaDLC -> minaMinhocaDLC m
    DinamiteDLC -> dinamiteMinhocaDLC m
    FlameTrower -> flameMinhocaDLC m
-- | Atualiza a quantidade de munições disponíveis de uma minhoca para uma dada arma.
atualizaQuantidadeArmaMinhoca :: TipoArmaDLC -> MinhocaDLC -> Int -> MinhocaDLC
atualizaQuantidadeArmaMinhoca tipo m i = case tipo of
    JetpackDLC -> m {jetpackMinhocaDLC = jetpackMinhocaDLC m + i}
    EscavadoraDLC -> m {escavadoraMinhocaDLC = escavadoraMinhocaDLC m + i}
    BazucaDLC -> m {bazucaMinhocaDLC = bazucaMinhocaDLC m + i}
    MinaDLC -> m {minaMinhocaDLC = minaMinhocaDLC m + i}
    DinamiteDLC -> m {dinamiteMinhocaDLC = dinamiteMinhocaDLC m + i}
    FlameTrower -> m {flameMinhocaDLC = flameMinhocaDLC m + i}


listaDonos :: [ObjetoDLC] -> [(TipoArmaDLC, Int)]
listaDonos [] = []
listaDonos (h:t)
  | ehDisparo h = (tipoDisparoDLC h, donoDisparoDLC h) : listaDonos t
  | otherwise   = listaDonos t



-- | Verifica se um tipo de terreno é destrutível, i.e., pode ser destruído por explosões.
--
-- __NB:__ Apenas @Terra@ é destrutível.
eTerrenoDestrutivel :: TerrenoDLC -> Bool
eTerrenoDestrutivel t = t == TerraDLC || t == Gelo

-- | Verifica se um tipo de terreno é opaco, i.e., não permite que objetos ou minhocas se encontrem por cima dele.
--
-- __NB:__ Apenas @Terra@ ou @Pedra@ são opacos.
eTerrenoOpaco :: TerrenoDLC -> Bool
eTerrenoOpaco t = t == PedraDLC || t == TerraDLC || t == Gelo

-- | Verifica se uma posição do mapa está livre, i.e., pode ser ocupada por um objeto ou minhoca.
--  
-- __NB:__ Uma posição está livre se não contiver um terreno opaco.
ePosicaoMapaLivre :: Posicao -> MapaDLC -> Bool
ePosicaoMapaLivre (_,_) [] = False
ePosicaoMapaLivre (0,y) (h:_) = encontraLinhaMapa y h
ePosicaoMapaLivre (x,y) (_:t) = ePosicaoMapaLivre (x-1,y) t

-- | Verifica se uma posição do estado está livre, i.e., pode ser ocupada por um objeto ou minhoca.
--
-- __NB:__ Uma posição está livre se o mapa estiver livre e se não estiver já uma minhoca ou um barril nessa posição.
ePosicaoEstadoLivre :: Posicao -> EstadoDLC -> Bool
ePosicaoEstadoLivre pos estado =
    ePosicaoMapaLivre pos mapa && not (existeMinhoca pos minhocas) && not (existeBarril pos objetos)
  where
    mapa = mapaEstadoDLC estado
    objetos = objetosEstadoDLC estado
    minhocas = minhocasEstadoDLC estado

-- | Destrói uma dada posição no mapa (tipicamente como consequência de uma explosão).
--
-- __NB__: Só terrenos @Terra@ pode ser destruídos.
-- Assumo que seja destruir e substituir por Ar
destroiPosicao :: Posicao -> MapaDLC -> MapaDLC
destroiPosicao pos mapa = let ter = encontraPosicaoMatriz pos mapa
                          in case ter of
                            Just Gelo -> atualizaPosicaoMatriz pos AguaDLC mapa
                            Just TerraDLC -> atualizaPosicaoMatriz pos ArDLC mapa
                            _ -> atualizaPosicaoMatriz pos ArDLC mapa
-- |Adiciona um novo objeto a um estado.
--
-- __NB__: A posição onde é inserido não é relevante.
adicionaObjeto :: ObjetoDLC -> EstadoDLC -> EstadoDLC
adicionaObjeto obj estado = estado {objetosEstadoDLC = obj : objetosEstadoDLC estado}

-- * Auxiliares 

-- | Verifica se numa linha do 'MapaDLC' a 'Posicao' dada é livre (não 'Opaca').(Utilizado na função 'ePosicaoMapaLivre')
encontraLinhaMapa :: Int -> [TerrenoDLC] -> Bool
encontraLinhaMapa _ [] = False
encontraLinhaMapa 0 (h:_) = not (eTerrenoOpaco h)
encontraLinhaMapa i (_:t) = encontraLinhaMapa (i-1) t


-- | Verifica se numa lista de minhocas já existe uma 'MinhocaDLC' na dada 'Posicao'.(Utilizado na função 'ePosicaoEstadoLivre')
existeMinhoca :: Posicao -> [MinhocaDLC] -> Bool
existeMinhoca _ [] = False
existeMinhoca (x,y) (h:t) = let pos = posicaoMinhocaDLC h
                            in if pos == Just (x,y) then True else existeMinhoca (x,y) t

-- | Verifica se numa lista de minhocas já existe uma 'MinhocaDLC' na dada 'Posicao'.(Utilizado na função 'ePosicaoEstadoLivre')
existeMinhocaViva :: Posicao -> [MinhocaDLC] -> Bool
existeMinhocaViva _ [] = False
existeMinhocaViva (x,y) (h:t) = let pos = posicaoMinhocaDLC h
                            in if pos == Just (x,y) && vidaMinhocaDLC h /= MortaDLC then True else existeMinhoca (x,y) t


-- | Verifica se numa lista de objetos já existe um 'Barril' na dada 'Posicao'.(Utilizado na função 'ePosicaoEstadoLivre')                   
existeBarril :: Posicao -> [ObjetoDLC] -> Bool
existeBarril _ [] = False
existeBarril pos (h:t) =
  case h of
    BarrilDLC p _ -> if p == pos then True else existeBarril pos t
    _ ->  existeBarril pos t

-- | Verifica se existe um dado objeto numa posicao e se existir devolve o mesmo
existeObjeto :: Posicao -> [ObjetoDLC] -> Maybe ObjetoDLC
existeObjeto _ [] = Nothing
existeObjeto p (h:t) = if posicaoObjeto h == p then Just h else existeObjeto p t

-- | Verifica se um 'ObjetoDLC' é um 'Disparo'.(Utilizado na função 'existeBarril')
ehDisparo :: ObjetoDLC -> Bool
ehDisparo DisparoDLC{} = True
ehDisparo _ = False



-- | Verifica se numa lista de objetos já existe um 'Disparo' feito para uma dada arma('TipoArmaDLC') por uma dada minhoca('NumMinhoca').
minhocaTemDisparo :: TipoArmaDLC -> NumMinhoca -> [ObjetoDLC] -> Bool
minhocaTemDisparo _ _ [] = False
minhocaTemDisparo tipo indiceMinhoca (h:ls) = if i == indiceMinhoca && t == tipo then True else minhocaTemDisparo tipo indiceMinhoca ls
    where
        i = donoDisparoDLC h
        t = tipoDisparoDLC h

{-| Verifica se a vida da minhoca é valida


== __Exemplos:__
>>> verificaVida MinhocaDLC{posicaoMinhocaDLC=Just (3,0), vidaMinhocaDLC=Morta, jetpackMinhoca=100, escavadoraMinhoca=200, bazucaMinhoca=150, minaMinhoca=3, dinamiteMinhoca=1}
True
>>> verificaVida MinhocaDLC{posicaoMinhocaDLC=Just (3,0), vidaMinhocaDLC=Viva 150, jetpackMinhoca=100, escavadoraMinhoca=200, bazucaMinhoca=150, minaMinhoca=3, dinamiteMinhoca=1}
False

-}
verificaVidaDLC :: MinhocaDLC -> Bool
verificaVidaDLC m = case vida of
                    MortaDLC -> True
                    VivaDLC a -> a >= 0 && a <= 100
    where
        vida = vidaMinhocaDLC m

-- | Verifica se uma minhoca esta viva
eMinhocaVivaDLC :: MinhocaDLC -> Bool
eMinhocaVivaDLC m = case vidaMinhocaDLC m of
                    MortaDLC -> False
                    VivaDLC 0 -> False
                    VivaDLC _ -> True



-- * Tipos de dados

-- * Funções não-recursivas.

-- | Verifica se o indice pertence à lista.
eIndiceListaValido :: Int -> [a] -> Bool
eIndiceListaValido x a = (x <= length a -1) && (x>(-1))

-- | Calcula a dimensão de uma matriz.
--
-- __NB:__ Note que não existem matrizes de dimensão /m * 0/ ou /0 * n/, e que qualquer matriz vazia deve ter dimensão /0 * 0/.
dimensaoMatriz :: Matriz a -> Dimensao
dimensaoMatriz a = (length a - 1, length (head a) - 1)

-- | Verifica se a posição pertence à matriz.
ePosicaoMatrizValida :: Posicao -> Matriz a -> Bool
ePosicaoMatrizValida (a,b) m = a <= length m - 1 && b <= length (head m) - 1 && a >= 0 && b >= 0

-- | Move uma posição uma unidade no sentido de uma direção.
movePosicao :: Direcao -> Posicao -> Posicao
movePosicao d (x, y) = case d of
    Norte     -> (x - 1, y)
    Sul       -> (x + 1, y)
    Este      -> (x, y + 1)
    Oeste     -> (x, y - 1)
    Nordeste  -> (x - 1, y + 1)
    Noroeste  -> (x - 1, y - 1)
    Sudeste   -> (x + 1, y + 1)
    Sudoeste  -> (x + 1, y - 1)




-- | Versão da função 'movePosicao' que garante que o movimento não se desloca para fora de uma janela.
--
-- __NB:__ Considere uma janela retangular com origem no canto superior esquerdo definida como uma matriz. A função recebe a dimensao da janela.
movePosicaoJanela :: Dimensao -> Direcao -> Posicao -> Posicao
movePosicaoJanela (dy,dx) dir pos =
    if (yf < dy) && (xf < dx) && (yf >= 0) && (xf >= 0)
        then (xf, yf)
        else pos
    where
        (yf, xf) = movePosicao dir pos


-- | Converte uma posição no referencial em que a origem é no canto superior esquerdo da janela numa posição em que a origem passa a estar no centro da janela.
--
-- __NB:__ Considere posições válidas. Efetue arredondamentos como achar necessário.
origemAoCentro :: Dimensao -> Posicao -> Posicao
origemAoCentro (dy, dx) (y, x) = (y - div dy 2, x - div dx 2)



-- | Roda um par (posição,direção) 45% para a direita.
--
-- __NB:__ Vendo um par (posição,direção) como um vector, cria um novo vetor do desto com a próxima direção da rosa dos ventos rodando para a direita.
--
-- <<https://haslab.github.io/Teaching/LI1/2526/img/rodaposicaodirecao.png>>

rodaPosicaoDirecao :: (Posicao,Direcao) -> (Posicao,Direcao)
rodaPosicaoDirecao (pos, d) = let p = movePosicao d pos
                              in case d of
                                    Norte    -> (p,Sul)
                                    Sul      -> (p,Norte)
                                    Este     -> (movePosicao Sudeste pos,Sudeste)
                                    Oeste    -> (movePosicao Sudoeste pos,Sudoeste)
                                    Nordeste -> (p,Este)
                                    Sudeste  -> (p,Sul)
                                    Noroeste -> (p,Oeste)
                                    Sudoeste -> (p,Sul)



-- * Funções recursivas.

-- | Devolve o elemento num dado índice de uma lista.
--
-- __NB:__ Retorna @Nothing@ se o índice não existir.
encontraIndiceLista :: Int -> [a] -> Maybe a
encontraIndiceLista _ [] = Nothing
encontraIndiceLista x (h:t)
  | x < 0 = Nothing
  | x == 0 = Just h
  | otherwise = encontraIndiceLista (x-1) t


-- | Modifica um elemento num dado índice.
--
-- __NB:__ Devolve a própria lista se o elemento não existir.
atualizaIndiceLista :: Int -> a -> [a] -> [a]
atualizaIndiceLista _ _ [] = []
atualizaIndiceLista 0 n (_:t) = n : t
atualizaIndiceLista i n (h:t)= h : atualizaIndiceLista (i-1) n t



-- | Devolve o elemento numa dada posição de uma matriz.
--
-- __NB:__ Retorna @Nothing@ se a posição não existir.
encontraPosicaoMatriz :: Posicao -> Matriz a -> Maybe a
encontraPosicaoMatriz _ [] = Nothing
encontraPosicaoMatriz (0,c) (h:_) = encontraLista c h
encontraPosicaoMatriz (l,c) (_:t) = encontraPosicaoMatriz (l-1,c) t






-- | Modifica um elemento numa dada posição de uma matriz.
--
-- __NB:__ Devolve a própria matriz se o elemento não existir.

atualizaPosicaoMatriz :: Posicao -> a -> Matriz a -> Matriz a
atualizaPosicaoMatriz _ _ [] = []
atualizaPosicaoMatriz (0,c) x (h:t) = substituirLista c x h : t
atualizaPosicaoMatriz (l,c) x (h:t) = h : atualizaPosicaoMatriz (l - 1, c) x t



-- | Verifica se uma matriz é válida, no sentido em que modela um rectângulo.
--
-- __NB:__ Todas as linhas devem ter o mesmo número de colunas. 
eMatrizValida :: Matriz a -> Bool
eMatrizValida [] = False
eMatrizValida [_] = True
eMatrizValida (h1:h2:t) = length h1 == length h2 && eMatrizValida (h2:t)


-- * Auxiliares

-- | Devolve a 'Direcao' oposta à dada.

direcaoOposta :: Direcao -> Direcao
direcaoOposta Norte    = Sul
direcaoOposta Sul      = Norte
direcaoOposta Este     = Oeste
direcaoOposta Oeste    = Este
direcaoOposta Nordeste = Sudoeste
direcaoOposta Sudoeste = Nordeste
direcaoOposta Noroeste = Sudeste
direcaoOposta Sudeste  = Noroeste

-- | Aplica uma sequência de movimentações a uma posição, pela ordem em que ocorrem na lista.
moveDirecoesPosicao :: [Direcao] -> Posicao -> Posicao
moveDirecoesPosicao [] pos = pos
moveDirecoesPosicao (h:t) pos = moveDirecoesPosicao t (movePosicao h pos)


-- | Aplica a mesma movimentação a uma lista de posições.
moveDirecaoPosicoes :: Direcao -> [Posicao] -> [Posicao]
moveDirecaoPosicoes _ [] = []
moveDirecaoPosicoes dir (h:t) = movePosicao dir h : moveDirecaoPosicoes dir t

-- | Função auxiliar de 'encontraPosicaoMatriz'. Devolve o elemento numa dada posição de uma lista.
encontraLista :: Int -> [a] -> Maybe a
encontraLista _ [] = Nothing
encontraLista i (h:t) |i == 0 = Just h
                      |otherwise = encontraLista (i-1) t

-- | Função auxiliar de 'atualizaPosicaoMatriz'. Substitui o elemento numa dada posição de uma lista.
substituirLista :: Int -> a -> [a] -> [a]
substituirLista _ _ [] = []
substituirLista 0 x (_:t) = x : t
substituirLista i x (h:t) = h : substituirLista (i - 1) x t


-- | Extrai a componente horizontal (Este/Oeste) de uma direção
getXWayDLC :: Direcao -> Maybe Direcao
getXWayDLC d = case d of
  Oeste     -> Just Oeste
  Este      -> Just Este
  Noroeste  -> Just Oeste
  Sudoeste  -> Just Oeste
  Nordeste  -> Just Este
  Sudeste   -> Just Este
  _         -> Nothing

-- | Devolve as posicoes ao redor do personagem
posicoes8Axis :: Posicao -> [Posicao]
posicoes8Axis pos =
  map (\d -> movePosicao d pos)
      [ Norte, Sul, Este, Oeste
      , Nordeste, Noroeste, Sudeste, Sudoeste
      ]
