{-|
Module      : Auxiliar
Description : Funções auxiliares para manipulação de estados, mapas e objetos do jogo.

Módulo contendo funções auxiliares utilizadas nas diversas tarefas de LI1\/LP1 em 2025\/26.

-}
module Auxiliar where


import DataDLC( Matriz, Dimensao,VidaMinhocaDLC(VivaDLC, MortaDLC), posicaoObjeto, tipoDisparoDLC,donoDisparoDLC, TerrenoDLC(ArDLC, PedraDLC, TerraDLC, Gelo, AguaDLC), objetosEstadoDLC,TipoArmaDLC(FlameTrower,JetpackDLC, EscavadoraDLC, BazucaDLC, MinaDLC, DinamiteDLC), EstadoDLC(mapaEstadoDLC, objetosEstadoDLC, minhocasEstadoDLC), MapaDLC, ObjetoDLC(BarrilDLC, DisparoDLC), MinhocaDLC(posicaoMinhocaDLC, vidaMinhocaDLC,jetpackMinhocaDLC, escavadoraMinhocaDLC, minaMinhocaDLC, dinamiteMinhocaDLC, bazucaMinhocaDLC, flameMinhocaDLC))
import Labs2025(NumMinhoca,Posicao, Direcao(Norte,Este,Oeste,Sul,Nordeste,Noroeste,Sudoeste,Sudeste))


-- * Funções de Minhocas

{-| Recebe uma lista e filtra as minhocas válidas (com posição e vivas).

Funcionalidade:

* Filtra apenas minhocas que têm posição definida ('Just')
* Verifica se a minhoca está viva através de 'eMinhocaVivaDLC'
* Remove minhocas sem posição ou mortas da lista

== __Exemplos:__
>>> getMinhocasValidasDLC [MinhocaDLC{posicaoMinhocaDLC=Just (1,1), vidaMinhocaDLC=VivaDLC 100, ...}, MinhocaDLC{posicaoMinhocaDLC=Nothing, vidaMinhocaDLC=MortaDLC, ...}]
[MinhocaDLC{posicaoMinhocaDLC=Just (1,1), vidaMinhocaDLC=VivaDLC 100, ...}]
-}
getMinhocasValidasDLC :: [MinhocaDLC] -> [MinhocaDLC]
getMinhocasValidasDLC [] = []
getMinhocasValidasDLC (h:t) =
  if posicaoMinhocaDLC h /= Nothing && eMinhocaVivaDLC h
    then h : getMinhocasValidasDLC t
    else getMinhocasValidasDLC t


{-| Retorna a quantidade de munições disponíveis de uma minhoca para uma dada arma.

Funcionalidade:

* Consulta o campo correspondente ao tipo de arma na minhoca
* Retorna o número de munições restantes

== __Exemplos:__
>>> encontraQuantidadeArmaMinhoca BazucaDLC MinhocaDLC{bazucaMinhocaDLC=5, ...}
5
-}
encontraQuantidadeArmaMinhoca :: TipoArmaDLC -> MinhocaDLC -> Int
encontraQuantidadeArmaMinhoca tipo m = case tipo of
    JetpackDLC -> jetpackMinhocaDLC m
    EscavadoraDLC -> escavadoraMinhocaDLC m
    BazucaDLC -> bazucaMinhocaDLC m
    MinaDLC -> minaMinhocaDLC m
    DinamiteDLC -> dinamiteMinhocaDLC m
    FlameTrower -> flameMinhocaDLC m

{-| Atualiza a quantidade de munições disponíveis de uma minhoca para uma dada arma.

Funcionalidade:

* Adiciona (ou subtrai, se negativo) a quantidade especificada à munição da arma
* Retorna uma nova minhoca com a munição atualizada

== __Exemplos:__
>>> atualizaQuantidadeArmaMinhoca BazucaDLC MinhocaDLC{bazucaMinhocaDLC=5, ...} (-1)
MinhocaDLC{bazucaMinhocaDLC=4, ...}
-}
atualizaQuantidadeArmaMinhoca :: TipoArmaDLC -> MinhocaDLC -> Int -> MinhocaDLC
atualizaQuantidadeArmaMinhoca tipo m i = case tipo of
    JetpackDLC -> m {jetpackMinhocaDLC = jetpackMinhocaDLC m + i}
    EscavadoraDLC -> m {escavadoraMinhocaDLC = escavadoraMinhocaDLC m + i}
    BazucaDLC -> m {bazucaMinhocaDLC = bazucaMinhocaDLC m + i}
    MinaDLC -> m {minaMinhocaDLC = minaMinhocaDLC m + i}
    DinamiteDLC -> m {dinamiteMinhocaDLC = dinamiteMinhocaDLC m + i}
    FlameTrower -> m {flameMinhocaDLC = flameMinhocaDLC m + i}


{-| Extrai uma lista de tuplos (tipo de arma, dono) dos disparos presentes numa lista de objetos.

Funcionalidade:

* Percorre a lista de objetos e identifica os disparos
* Cria tuplos com o tipo de arma e o índice do dono para cada disparo
* Ignora objetos que não são disparos

== __Exemplos:__
>>> listaDonos [DisparoDLC{tipoDisparoDLC=BazucaDLC, donoDisparoDLC=0}, BarrilDLC (1,1) 3]
[(BazucaDLC,0)]
-}
listaDonos :: [ObjetoDLC] -> [(TipoArmaDLC, Int)]
listaDonos [] = []
listaDonos (h:t)
  | ehDisparo h = (tipoDisparoDLC h, donoDisparoDLC h) : listaDonos t
  | otherwise   = listaDonos t



-- * Funções de Terreno e Validação

{-| Verifica se um tipo de terreno é destrutível, i.e., pode ser destruído por explosões.

__NB:__ Apenas @TerraDLC@ e @Gelo@ são destrutíveis.

== __Exemplos:__
>>> eTerrenoDestrutivel TerraDLC
True
>>> eTerrenoDestrutivel PedraDLC
False
-}
eTerrenoDestrutivel :: TerrenoDLC -> Bool
eTerrenoDestrutivel t = t == TerraDLC || t == Gelo

{-| Verifica se um tipo de terreno é opaco, i.e., não permite que objetos ou minhocas se encontrem por cima dele.

__NB:__ Apenas @TerraDLC@, @PedraDLC@ ou @Gelo@ são opacos.

== __Exemplos:__
>>> eTerrenoOpaco PedraDLC
True
>>> eTerrenoOpaco ArDLC
False
-}
eTerrenoOpaco :: TerrenoDLC -> Bool
eTerrenoOpaco t = t == PedraDLC || t == TerraDLC || t == Gelo

{-| Verifica se uma posição do mapa está livre, i.e., pode ser ocupada por um objeto ou minhoca.

__NB:__ Uma posição está livre se não contiver um terreno opaco.

Funcionalidade:

* Verifica se a posição no mapa não contém terreno opaco
* Utiliza 'encontraLinhaMapa' para verificar a linha específica

== __Exemplos:__
>>> ePosicaoMapaLivre (0,0) [[ArDLC,ArDLC],[TerraDLC,TerraDLC]]
True
>>> ePosicaoMapaLivre (1,0) [[ArDLC,ArDLC],[TerraDLC,TerraDLC]]
False
-}
ePosicaoMapaLivre :: Posicao -> MapaDLC -> Bool
ePosicaoMapaLivre (_,_) [] = False
ePosicaoMapaLivre (0,y) (h:_) = encontraLinhaMapa y h
ePosicaoMapaLivre (x,y) (_:t) = ePosicaoMapaLivre (x-1,y) t

{-| Verifica se uma posição do estado está livre, i.e., pode ser ocupada por um objeto ou minhoca.

__NB:__ Uma posição está livre se o mapa estiver livre e se não estiver já uma minhoca ou um barril nessa posição.

Funcionalidade:

* Verifica se a posição está livre no mapa ('ePosicaoMapaLivre')
* Verifica se não existe minhoca na posição ('existeMinhoca')
* Verifica se não existe barril na posição ('existeBarril')

== __Exemplos:__
>>> ePosicaoEstadoLivre (0,0) EstadoDLC{mapaEstadoDLC=[[ArDLC]], objetosEstadoDLC=[], minhocasEstadoDLC=[]}
True
-}
ePosicaoEstadoLivre :: Posicao -> EstadoDLC -> Bool
ePosicaoEstadoLivre pos estado =
    ePosicaoMapaLivre pos mapa && not (existeMinhoca pos minhocas) && not (existeBarril pos objetos)
  where
    mapa = mapaEstadoDLC estado
    objetos = objetosEstadoDLC estado
    minhocas = minhocasEstadoDLC estado

{-| Destrói uma dada posição no mapa (tipicamente como consequência de uma explosão).

__NB__: Só terrenos @TerraDLC@ e @Gelo@ podem ser destruídos. @Gelo@ transforma-se em @AguaDLC@, @TerraDLC@ transforma-se em @ArDLC@.

Funcionalidade:

* Identifica o tipo de terreno na posição
* Substitui @Gelo@ por @AguaDLC@
* Substitui @TerraDLC@ por @ArDLC@
* Outros terrenos são substituídos por @ArDLC@

== __Exemplos:__
>>> destroiPosicao (0,0) [[TerraDLC]]
[[ArDLC]]
>>> destroiPosicao (0,0) [[Gelo]]
[[AguaDLC]]
-}
destroiPosicao :: Posicao -> MapaDLC -> MapaDLC
destroiPosicao pos mapa = let ter = encontraPosicaoMatriz pos mapa
                          in case ter of
                            Just Gelo -> atualizaPosicaoMatriz pos AguaDLC mapa
                            Just TerraDLC -> atualizaPosicaoMatriz pos ArDLC mapa
                            _ -> atualizaPosicaoMatriz pos ArDLC mapa

{-| Adiciona um novo objeto a um estado.

__NB__: A posição onde é inserido não é relevante. O objeto é adicionado ao início da lista de objetos.

Funcionalidade:

* Adiciona o objeto à lista de objetos do estado
* Mantém todos os outros campos do estado inalterados

== __Exemplos:__
>>> adicionaObjeto BarrilDLC (1,1) 3 EstadoDLC{objetosEstadoDLC=[], ...}
EstadoDLC{objetosEstadoDLC=[BarrilDLC (1,1) 3], ...}
-}
adicionaObjeto :: ObjetoDLC -> EstadoDLC -> EstadoDLC
adicionaObjeto obj estado = estado {objetosEstadoDLC = obj : objetosEstadoDLC estado}

-- * Auxiliares de Posição e Verificação

{-| Verifica se numa linha do 'MapaDLC' a 'Posicao' dada é livre (não opaca).

Funcionalidade:

* Percorre uma linha do mapa até encontrar a coluna especificada
* Verifica se o terreno nessa posição não é opaco
* Utilizada na função 'ePosicaoMapaLivre'

== __Exemplos:__
>>> encontraLinhaMapa 0 [ArDLC,TerraDLC,ArDLC]
True
>>> encontraLinhaMapa 1 [ArDLC,TerraDLC,ArDLC]
False
-}
encontraLinhaMapa :: Int -> [TerrenoDLC] -> Bool
encontraLinhaMapa _ [] = False
encontraLinhaMapa 0 (h:_) = not (eTerrenoOpaco h)
encontraLinhaMapa i (_:t) = encontraLinhaMapa (i-1) t


{-| Verifica se numa lista de minhocas já existe uma 'MinhocaDLC' na dada 'Posicao'.

Funcionalidade:

* Percorre a lista de minhocas
* Compara a posição de cada minhoca com a posição dada
* Utilizada na função 'ePosicaoEstadoLivre'

== __Exemplos:__
>>> existeMinhoca (1,1) [MinhocaDLC{posicaoMinhocaDLC=Just (1,1), ...}]
True
>>> existeMinhoca (0,0) [MinhocaDLC{posicaoMinhocaDLC=Just (1,1), ...}]
False
-}
existeMinhoca :: Posicao -> [MinhocaDLC] -> Bool
existeMinhoca _ [] = False
existeMinhoca (x,y) (h:t) = let pos = posicaoMinhocaDLC h
                            in if pos == Just (x,y) then True else existeMinhoca (x,y) t

{-| Verifica se numa lista de minhocas já existe uma 'MinhocaDLC' viva na dada 'Posicao'.

Funcionalidade:

* Semelhante a 'existeMinhoca', mas também verifica se a minhoca está viva
* Retorna True apenas se houver uma minhoca viva na posição

== __Exemplos:__
>>> existeMinhocaViva (1,1) [MinhocaDLC{posicaoMinhocaDLC=Just (1,1), vidaMinhocaDLC=VivaDLC 50, ...}]
True
>>> existeMinhocaViva (1,1) [MinhocaDLC{posicaoMinhocaDLC=Just (1,1), vidaMinhocaDLC=MortaDLC, ...}]
False
-}
existeMinhocaViva :: Posicao -> [MinhocaDLC] -> Bool
existeMinhocaViva _ [] = False
existeMinhocaViva (x,y) (h:t) = let pos = posicaoMinhocaDLC h
                            in if pos == Just (x,y) && vidaMinhocaDLC h /= MortaDLC then True else existeMinhoca (x,y) t


{-| Verifica se numa lista de objetos já existe um 'BarrilDLC' na dada 'Posicao'.

Funcionalidade:

* Percorre a lista de objetos
* Verifica se algum objeto do tipo 'BarrilDLC' está na posição especificada
* Utilizada na função 'ePosicaoEstadoLivre'

== __Exemplos:__
>>> existeBarril (1,1) [BarrilDLC (1,1) 3]
True
>>> existeBarril (0,0) [BarrilDLC (1,1) 3]
False
-}                  
existeBarril :: Posicao -> [ObjetoDLC] -> Bool
existeBarril _ [] = False
existeBarril pos (h:t) =
  case h of
    BarrilDLC p _ -> if p == pos then True else existeBarril pos t
    _ ->  existeBarril pos t

{-| Verifica se existe um dado objeto numa posição e se existir devolve o mesmo.

Funcionalidade:

* Percorre a lista de objetos
* Retorna 'Just' objeto se encontrar um na posição especificada
* Retorna 'Nothing' se não encontrar

== __Exemplos:__
>>> existeObjeto (1,1) [BarrilDLC (1,1) 3, DisparoDLC{posicaoObjeto=(2,2), ...}]
Just (BarrilDLC (1,1) 3)
-}
existeObjeto :: Posicao -> [ObjetoDLC] -> Maybe ObjetoDLC
existeObjeto _ [] = Nothing
existeObjeto p (h:t) = if posicaoObjeto h == p then Just h else existeObjeto p t

{-| Verifica se um 'ObjetoDLC' é um 'DisparoDLC'.

Funcionalidade:

* Usa pattern matching para identificar o tipo de objeto
* Retorna True se for um disparo, False caso contrário

== __Exemplos:__
>>> ehDisparo DisparoDLC{...}
True
>>> ehDisparo BarrilDLC (1,1) 3
False
-}
ehDisparo :: ObjetoDLC -> Bool
ehDisparo DisparoDLC{} = True
ehDisparo _ = False



{-| Verifica se numa lista de objetos já existe um 'DisparoDLC' feito para uma dada arma('TipoArmaDLC') por uma dada minhoca('NumMinhoca').

Funcionalidade:

* Percorre a lista de objetos
* Verifica se existe um disparo do tipo especificado feito pela minhoca especificada
* Útil para prevenir múltiplos disparos do mesmo tipo pela mesma minhoca

== __Exemplos:__
>>> minhocaTemDisparo BazucaDLC 0 [DisparoDLC{tipoDisparoDLC=BazucaDLC, donoDisparoDLC=0, ...}]
True
>>> minhocaTemDisparo MinaDLC 0 [DisparoDLC{tipoDisparoDLC=BazucaDLC, donoDisparoDLC=0, ...}]
False
-}
minhocaTemDisparo :: TipoArmaDLC -> NumMinhoca -> [ObjetoDLC] -> Bool
minhocaTemDisparo _ _ [] = False
minhocaTemDisparo tipo indiceMinhoca (h:ls) = if i == indiceMinhoca && t == tipo then True else minhocaTemDisparo tipo indiceMinhoca ls
    where
        i = donoDisparoDLC h
        t = tipoDisparoDLC h

{-| Verifica se a vida da minhoca é válida.

Funcionalidade:

* Verifica se a minhoca morta tem vida 'MortaDLC'
* Verifica se a minhoca viva tem vida entre 0 e 100
* Retorna False para valores de vida inválidos

== __Exemplos:__
>>> verificaVidaDLC MinhocaDLC{vidaMinhocaDLC=MortaDLC, ...}
True
>>> verificaVidaDLC MinhocaDLC{vidaMinhocaDLC=VivaDLC 50, ...}
True
>>> verificaVidaDLC MinhocaDLC{vidaMinhocaDLC=VivaDLC 150, ...}
False
-}
verificaVidaDLC :: MinhocaDLC -> Bool
verificaVidaDLC m = case vida of
                    MortaDLC -> True
                    VivaDLC a -> a >= 0 && a <= 100
    where
        vida = vidaMinhocaDLC m

{-| Verifica se uma minhoca está viva.

Funcionalidade:

* Retorna False se a vida for 'MortaDLC'
* Retorna False se a vida for 'VivaDLC 0'
* Retorna True caso contrário

== __Exemplos:__
>>> eMinhocaVivaDLC MinhocaDLC{vidaMinhocaDLC=VivaDLC 50, ...}
True
>>> eMinhocaVivaDLC MinhocaDLC{vidaMinhocaDLC=MortaDLC, ...}
False
>>> eMinhocaVivaDLC MinhocaDLC{vidaMinhocaDLC=VivaDLC 0, ...}
False
-}
eMinhocaVivaDLC :: MinhocaDLC -> Bool
eMinhocaVivaDLC m = case vidaMinhocaDLC m of
                    MortaDLC -> False
                    VivaDLC 0 -> False
                    VivaDLC _ -> True



-- * Funções não-recursivas

{-| Verifica se o índice pertence à lista.

Funcionalidade:

* Verifica se o índice é não-negativo
* Verifica se o índice é menor que o comprimento da lista

== __Exemplos:__
>>> eIndiceListaValido 0 [1,2,3]
True
>>> eIndiceListaValido 5 [1,2,3]
False
>>> eIndiceListaValido (-1) [1,2,3]
False
-}
eIndiceListaValido :: Int -> [a] -> Bool
eIndiceListaValido x a = (x <= length a -1) && (x>(-1))

{-| Calcula a dimensão de uma matriz.

__NB:__ Note que não existem matrizes de dimensão /m * 0/ ou /0 * n/, e que qualquer matriz vazia deve ter dimensão /0 * 0/.

Funcionalidade:

* Retorna um par (número de linhas - 1, número de colunas - 1)
* Os valores são decrementados para compatibilidade com indexação baseada em 0

== __Exemplos:__
>>> dimensaoMatriz [[1,2,3],[4,5,6]]
(1,2)
-}
dimensaoMatriz :: Matriz a -> Dimensao
dimensaoMatriz a = (length a - 1, length (head a) - 1)

{-| Verifica se a posição pertence à matriz.

Funcionalidade:

* Verifica se as coordenadas são não-negativas
* Verifica se as coordenadas estão dentro dos limites da matriz

== __Exemplos:__
>>> ePosicaoMatrizValida (0,0) [[1,2],[3,4]]
True
>>> ePosicaoMatrizValida (2,2) [[1,2],[3,4]]
False
>>> ePosicaoMatrizValida (-1,0) [[1,2],[3,4]]
False
-}
ePosicaoMatrizValida :: Posicao -> Matriz a -> Bool
ePosicaoMatrizValida (a,b) m = a <= length m - 1 && b <= length (head m) - 1 && a >= 0 && b >= 0

{-| Move uma posição uma unidade no sentido de uma direção.

Funcionalidade:

* Recebe uma direção e uma posição
* Retorna a nova posição após o movimento
* Suporta as 8 direções cardeais e intercardeais

== __Exemplos:__
>>> movePosicao Norte (1,1)
(0,1)
>>> movePosicao Sudeste (1,1)
(2,2)
>>> movePosicao Oeste (1,1)
(1,0)
-}
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




{-| Versão da função 'movePosicao' que garante que o movimento não se desloca para fora de uma janela.

__NB:__ Considere uma janela retangular com origem no canto superior esquerdo definida como uma matriz. A função recebe a dimensão da janela.

Funcionalidade:

* Move a posição na direção especificada
* Verifica se a nova posição está dentro da janela
* Retorna a posição original se o movimento sair dos limites

== __Exemplos:__
>>> movePosicaoJanela (5,5) Norte (0,2)
(0,2)
>>> movePosicaoJanela (5,5) Sul (2,2)
(3,2)
-}
movePosicaoJanela :: Dimensao -> Direcao -> Posicao -> Posicao
movePosicaoJanela (dy,dx) dir pos =
    if (yf < dy) && (xf < dx) && (yf >= 0) && (xf >= 0)
        then (xf, yf)
        else pos
    where
        (yf, xf) = movePosicao dir pos


{-| Converte uma posição no referencial em que a origem é no canto superior esquerdo da janela numa posição em que a origem passa a estar no centro da janela.

__NB:__ Considere posições válidas. Efetue arredondamentos como achar necessário.

Funcionalidade:

* Recebe a dimensão da janela e uma posição
* Subtrai metade da dimensão para centrar a origem
* Útil para conversão de coordenadas em sistemas gráficos

== __Exemplos:__
>>> origemAoCentro (10,10) (5,5)
(0,0)
>>> origemAoCentro (10,10) (7,3)
(2,-2)
-}
origemAoCentro :: Dimensao -> Posicao -> Posicao
origemAoCentro (dy, dx) (y, x) = (y - div dy 2, x - div dx 2)



{-| Roda um par (posição,direção) 45° para a direita.

__NB:__ Vendo um par (posição,direção) como um vetor, cria um novo vetor do resto com a próxima direção da rosa dos ventos rodando para a direita.

<<https://haslab.github.io/Teaching/LI1/2526/img/rodaposicaodirecao.png>>

Funcionalidade:

* Move a posição na direção atual
* Roda a direção 45° no sentido horário
* Útil para algoritmos de rotação e pathfinding

== __Exemplos:__
>>> rodaPosicaoDirecao ((1,1), Norte)
((0,1), Sul)
>>> rodaPosicaoDirecao ((1,1), Este)
((2,2), Sudeste)
-}
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



-- * Funções recursivas

{-| Devolve o elemento num dado índice de uma lista.

__NB:__ Retorna @Nothing@ se o índice não existir.

Funcionalidade:

* Percorre a lista até encontrar o índice especificado
* Retorna 'Nothing' para índices negativos ou fora dos limites
* Retorna 'Just' elemento se o índice for válido

== __Exemplos:__
>>> encontraIndiceLista 0 [1,2,3]
Just 1
>>> encontraIndiceLista 2 [1,2,3]
Just 3
>>> encontraIndiceLista 5 [1,2,3]
Nothing
>>> encontraIndiceLista (-1) [1,2,3]
Nothing
-}
encontraIndiceLista :: Int -> [a] -> Maybe a
encontraIndiceLista _ [] = Nothing
encontraIndiceLista x (h:t)
  | x < 0 = Nothing
  | x == 0 = Just h
  | otherwise = encontraIndiceLista (x-1) t


{-| Modifica um elemento num dado índice.

__NB:__ Devolve a própria lista se o elemento não existir.

Funcionalidade:

* Percorre a lista até encontrar o índice
* Substitui o elemento no índice especificado
* Mantém os outros elementos inalterados

== __Exemplos:__
>>> atualizaIndiceLista 0 10 [1,2,3]
[10,2,3]
>>> atualizaIndiceLista 2 10 [1,2,3]
[1,2,10]
>>> atualizaIndiceLista 5 10 [1,2,3]
[1,2,3]
-}
atualizaIndiceLista :: Int -> a -> [a] -> [a]
atualizaIndiceLista _ _ [] = []
atualizaIndiceLista 0 n (_:t) = n : t
atualizaIndiceLista i n (h:t)= h : atualizaIndiceLista (i-1) n t



{-| Devolve o elemento numa dada posição de uma matriz.

__NB:__ Retorna @Nothing@ se a posição não existir.

Funcionalidade:

* Navega pelas linhas da matriz
* Utiliza 'encontraLista' para localizar a coluna
* Retorna 'Nothing' se a posição for inválida

== __Exemplos:__
>>> encontraPosicaoMatriz (0,0) [[1,2],[3,4]]
Just 1
>>> encontraPosicaoMatriz (1,1) [[1,2],[3,4]]
Just 4
>>> encontraPosicaoMatriz (2,2) [[1,2],[3,4]]
Nothing
-}
encontraPosicaoMatriz :: Posicao -> Matriz a -> Maybe a
encontraPosicaoMatriz _ [] = Nothing
encontraPosicaoMatriz (0,c) (h:_) = encontraLista c h
encontraPosicaoMatriz (l,c) (_:t) = encontraPosicaoMatriz (l-1,c) t






{-| Modifica um elemento numa dada posição de uma matriz.

__NB:__ Devolve a própria matriz se o elemento não existir.

Funcionalidade:

* Navega pelas linhas da matriz
* Utiliza 'substituirLista' para atualizar a coluna
* Mantém a estrutura da matriz inalterada

== __Exemplos:__
>>> atualizaPosicaoMatriz (0,0) 10 [[1,2],[3,4]]
[[10,2],[3,4]]
>>> atualizaPosicaoMatriz (1,1) 10 [[1,2],[3,4]]
[[1,2],[3,10]]
-}
atualizaPosicaoMatriz :: Posicao -> a -> Matriz a -> Matriz a
atualizaPosicaoMatriz _ _ [] = []
atualizaPosicaoMatriz (0,c) x (h:t) = substituirLista c x h : t
atualizaPosicaoMatriz (l,c) x (h:t) = h : atualizaPosicaoMatriz (l - 1, c) x t



{-| Verifica se uma matriz é válida, no sentido em que modela um retângulo.

__NB:__ Todas as linhas devem ter o mesmo número de colunas.

Funcionalidade:

* Verifica recursivamente se todas as linhas têm o mesmo comprimento
* Retorna False para matriz vazia
* Retorna True se todas as linhas forem consistentes

== __Exemplos:__
>>> eMatrizValida [[1,2],[3,4]]
True
>>> eMatrizValida [[1,2],[3,4,5]]
False
>>> eMatrizValida []
False
-}
eMatrizValida :: Matriz a -> Bool
eMatrizValida [] = False
eMatrizValida [_] = True
eMatrizValida (h1:h2:t) = length h1 == length h2 && eMatrizValida (h2:t)


-- * Auxiliares de Direção

{-| Devolve a 'Direcao' oposta à dada.

Funcionalidade:

* Mapeia cada direção para a sua oposta
* Útil para calcular movimentos de retrocesso

== __Exemplos:__
>>> direcaoOposta Norte
Sul
>>> direcaoOposta Nordeste
Sudoeste
-}
direcaoOposta :: Direcao -> Direcao
direcaoOposta Norte    = Sul
direcaoOposta Sul      = Norte
direcaoOposta Este     = Oeste
direcaoOposta Oeste    = Este
direcaoOposta Nordeste = Sudoeste
direcaoOposta Sudoeste = Nordeste
direcaoOposta Noroeste = Sudeste
direcaoOposta Sudeste  = Noroeste

{-| Aplica uma sequência de movimentações a uma posição, pela ordem em que ocorrem na lista.

Funcionalidade:

* Aplica cada direção sucessivamente
* A ordem das direções é relevante
* Útil para calcular trajetórias

== __Exemplos:__
>>> moveDirecoesPosicao [Norte, Este] (1,1)
(0,2)
>>> moveDirecoesPosicao [Sul, Sul, Oeste] (0,0)
(2,-1)
-}
moveDirecoesPosicao :: [Direcao] -> Posicao -> Posicao
moveDirecoesPosicao [] pos = pos
moveDirecoesPosicao (h:t) pos = moveDirecoesPosicao t (movePosicao h pos)


{-| Aplica a mesma movimentação a uma lista de posições.

Funcionalidade:

* Move cada posição da lista na mesma direção
* Mantém a ordem das posições
* Útil para mover grupos de objetos

== __Exemplos:__
>>> moveDirecaoPosicoes Norte [(1,1), (2,2)]
[(0,1), (1,2)]
>>> moveDirecaoPosicoes Este [(0,0), (1,1), (2,2)]
[(0,1), (1,2), (2,3)]
-}
moveDirecaoPosicoes :: Direcao -> [Posicao] -> [Posicao]
moveDirecaoPosicoes _ [] = []
moveDirecaoPosicoes dir (h:t) = movePosicao dir h : moveDirecaoPosicoes dir t

{-| Função auxiliar de 'encontraPosicaoMatriz'. Devolve o elemento numa dada posição de uma lista.

Funcionalidade:

* Percorre uma lista até encontrar o índice
* Retorna 'Just' elemento ou 'Nothing'
* Usada para localizar colunas em matrizes

== __Exemplos:__
>>> encontraLista 0 [1,2,3]
Just 1
>>> encontraLista 2 [1,2,3]
Just 3
>>> encontraLista 5 [1,2,3]
Nothing
-}
encontraLista :: Int -> [a] -> Maybe a
encontraLista _ [] = Nothing
encontraLista i (h:t) |i == 0 = Just h
                      |otherwise = encontraLista (i-1) t

{-| Função auxiliar de 'atualizaPosicaoMatriz'. Substitui o elemento numa dada posição de uma lista.

Funcionalidade:

* Percorre uma lista até encontrar o índice
* Substitui o elemento no índice especificado
* Usada para atualizar colunas em matrizes

== __Exemplos:__
>>> substituirLista 0 10 [1,2,3]
[10,2,3]
>>> substituirLista 2 10 [1,2,3]
[1,2,10]
-}
substituirLista :: Int -> a -> [a] -> [a]
substituirLista _ _ [] = []
substituirLista 0 x (_:t) = x : t
substituirLista i x (h:t) = h : substituirLista (i - 1) x t


{-| Extrai a componente horizontal (Este/Oeste) de uma direção.

Funcionalidade:

* Identifica se a direção tem componente Este ou Oeste
* Retorna 'Nothing' para direções puramente verticais
* Útil para separar movimentos em eixos

== __Exemplos:__
>>> getXWayDLC Este
Just Este
>>> getXWayDLC Nordeste
Just Este
>>> getXWayDLC Norte
Nothing
-}
getXWayDLC :: Direcao -> Maybe Direcao
getXWayDLC d = case d of
  Oeste     -> Just Oeste
  Este      -> Just Este
  Noroeste  -> Just Oeste
  Sudoeste  -> Just Oeste
  Nordeste  -> Just Este
  Sudeste   -> Just Este
  _         -> Nothing

{-| Devolve as posições ao redor de uma posição central (8 direções).

Funcionalidade:

* Calcula as 8 posições adjacentes
* Inclui diagonais e cardeais
* Útil para verificar vizinhanças e colisões

== __Exemplos:__
>>> posicoes8Axis (1,1)
[(0,1),(2,1),(1,2),(1,0),(0,2),(0,0),(2,2),(2,0)]
-}
posicoes8Axis :: Posicao -> [Posicao]
posicoes8Axis pos =
  map (\d -> movePosicao d pos)
      [ Norte, Sul, Este, Oeste
      , Nordeste, Noroeste, Sudeste, Sudoeste
      ]