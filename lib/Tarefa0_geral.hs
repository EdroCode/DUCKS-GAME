{-|
Module      : Tarefa0_geral
Description : Funções auxiliares gerais.

Módulo que define funções genéricas sobre listas e matrizes.
-}



module Tarefa0_geral where



-- * Tipos de dados

-- | Uma matriz é um conjunto de elementos a duas dimensões.
--
-- Em notação matemática, é geralmente representada por:
--
-- <<https://haslab.github.io/Teaching/LI1/2526/img/matriz.png>>
type Matriz a = [[a]]

-- | Uma posição numa matriz é dada como um par (/linha/,/colunha/).
-- As coordenadas são dois números naturais e começam com (0,0) no canto superior esquerdo, com as linhas incrementando para baixo e as colunas incrementando para a direita.
--
-- <<https://haslab.github.io/Teaching/LI1/2526/img/posicaomatriz.png>>
type Posicao = (Int,Int)

-- | A dimensão de uma matrix dada como um par (/número de linhas/,/número de colunhas/).
type Dimensao = (Int,Int)

-- | Uma direção é dada pela rosa dos ventos. Ou seja, os 4 pontos cardeais e os 4 pontos colaterais.
--
-- <<https://haslab.github.io/Teaching/LI1/2526/img/rosadosventos.jpg>>
data Direcao = Norte | Nordeste | Este | Sudeste | Sul | Sudoeste | Oeste | Noroeste
    deriving (Eq,Ord,Show,Read,Enum)

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
rodaPosicaoDirecao (pos, d) = (p, novaDirecao) -- Retorna a tupla (Posicao, Direcao)
    where
        novaDirecao = case d of
            Norte    -> Nordeste
            Sul      -> Sudoeste
            Este     -> Sudeste
            Oeste    -> Noroeste
            Nordeste -> Este
            Sudeste  -> Sul
            Noroeste -> Norte
            Sudoeste -> Oeste

        p = movePosicao d pos

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
