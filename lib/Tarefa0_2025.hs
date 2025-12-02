{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant ==" #-}
{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Eta reduce" #-}
{-|
Module      : Tarefa0_2025
Description : Funções auxiliares.

Módulo que define funções auxiliares que serão úteis na resolução do trabalho prático de LI1\/LP1 em 2025\/26.
-}

-- | Este módulo 
module Tarefa0_2025 where
    
import Labs2025


-- | Retorna a quantidade de munições disponíveis de uma minhoca para uma dada arma.
encontraQuantidadeArmaMinhoca :: TipoArma -> Minhoca -> Int
encontraQuantidadeArmaMinhoca tipo m = case tipo of
    Jetpack -> jetpackMinhoca m
    Escavadora -> escavadoraMinhoca m
    Bazuca -> bazucaMinhoca m
    Mina -> minaMinhoca m
    Dinamite -> dinamiteMinhoca m

-- | Atualiza a quantidade de munições disponíveis de uma minhoca para uma dada arma.
atualizaQuantidadeArmaMinhoca :: TipoArma -> Minhoca -> Int -> Minhoca
atualizaQuantidadeArmaMinhoca tipo m i = case tipo of
    Jetpack -> m {jetpackMinhoca = jetpackMinhoca m + i}
    Escavadora -> m {escavadoraMinhoca = escavadoraMinhoca m + i}
    Bazuca -> m {bazucaMinhoca = bazucaMinhoca m + i}
    Mina -> m {minaMinhoca = minaMinhoca m + i}
    Dinamite -> m {dinamiteMinhoca = dinamiteMinhoca m + i}



-- | Verifica se um tipo de terreno é destrutível, i.e., pode ser destruído por explosões.
--
-- __NB:__ Apenas @Terra@ é destrutível.
eTerrenoDestrutivel :: Terreno -> Bool
eTerrenoDestrutivel t = if t == Terra then True else False

-- | Verifica se um tipo de terreno é opaco, i.e., não permite que objetos ou minhocas se encontrem por cima dele.
--
-- __NB:__ Apenas @Terra@ ou @Pedra@ são opacos.
eTerrenoOpaco :: Terreno -> Bool
eTerrenoOpaco t = if t == Pedra || t == Terra then True else False

-- | Verifica se uma posição do mapa está livre, i.e., pode ser ocupada por um objeto ou minhoca.
--  
-- __NB:__ Uma posição está livre se não contiver um terreno opaco.
ePosicaoMapaLivre :: Posicao -> Mapa -> Bool
ePosicaoMapaLivre (_,_) [] = False
ePosicaoMapaLivre (0,y) (h:_) = encontraLinhaMapa y h
ePosicaoMapaLivre (x,y) (_:t) = ePosicaoMapaLivre (x-1,y) t

-- | Verifica se uma posição do estado está livre, i.e., pode ser ocupada por um objeto ou minhoca.
--
-- __NB:__ Uma posição está livre se o mapa estiver livre e se não estiver já uma minhoca ou um barril nessa posição.
ePosicaoEstadoLivre :: Posicao -> Estado -> Bool
ePosicaoEstadoLivre pos estado =
    ePosicaoMapaLivre pos mapa && not (existeMinhoca pos minhocas) && not (existeBarril pos objetos)
  where
    mapa = mapaEstado estado
    objetos = objetosEstado estado
    minhocas = minhocasEstado estado

-- | Destrói uma dada posição no mapa (tipicamente como consequência de uma explosão).
--
-- __NB__: Só terrenos @Terra@ pode ser destruídos.
-- Assumo que seja destruir e substituir por Ar
destroiPosicao :: Posicao -> Mapa -> Mapa
destroiPosicao pos mapa = atualizaPosicaoMatriz pos Ar mapa

-- |Adiciona um novo objeto a um estado.
--
-- __NB__: A posição onde é inserido não é relevante.
adicionaObjeto :: Objeto -> Estado -> Estado
adicionaObjeto obj estado = estado {objetosEstado = obj : objetosEstado estado}

-- * Auxiliares 

-- | Verifica se numa linha do 'Mapa' a 'Posicao' dada é livre (não 'Opaca').(Utilizado na função 'ePosicaoMapaLivre')
encontraLinhaMapa :: Int -> [Terreno] -> Bool
encontraLinhaMapa _ [] = False
encontraLinhaMapa 0 (h:_) = not (eTerrenoOpaco h)
encontraLinhaMapa i (_:t) = encontraLinhaMapa (i-1) t 


-- | Verifica se numa lista de minhocas já existe uma 'Minhoca' na dada 'Posicao'.(Utilizado na função 'ePosicaoEstadoLivre')
existeMinhoca :: Posicao -> [Minhoca] -> Bool
existeMinhoca _ [] = False
existeMinhoca (x,y) (h:t) = let pos = posicaoMinhoca h
                            in if pos == Just (x,y) then True else existeMinhoca (x,y) t
                       
-- | Verifica se numa lista de objetos já existe um 'Barril' na dada 'Posicao'.(Utilizado na função 'ePosicaoEstadoLivre')                   
existeBarril :: Posicao -> [Objeto] -> Bool
existeBarril _ [] = False
existeBarril pos (h:t) =
  let p = posicaoObjeto h
  in if ehDisparo h == False && p == pos
              then True
              else existeBarril pos t

-- | Verifica se um 'Objeto' é um 'Disparo'.(Utilizado na função 'existeBarril')
ehDisparo :: Objeto -> Bool
ehDisparo Disparo{} = True
ehDisparo _ = False

-- | Devolve a 'Posição' de um 'Objeto'.(Utilizado na função 'existeBarril')
posicaoObjeto :: Objeto -> Posicao
posicaoObjeto d@(Disparo {})  = posicaoDisparo d
posicaoObjeto b@(Barril {}) = posicaoBarril b



-- | Verifica se numa lista de objetos já existe um 'Disparo' feito para uma dada arma('TipoArma') por uma dada minhoca('NumMinhoca').
minhocaTemDisparo :: TipoArma -> NumMinhoca -> [Objeto] -> Bool
minhocaTemDisparo _ _ [] = False 
minhocaTemDisparo tipo indiceMinhoca (h:ls) = if i == indiceMinhoca && t == tipo then True else minhocaTemDisparo tipo indiceMinhoca ls
    where
        i = donoDisparo h
        t = tipoDisparo h

{-| Verifica se a vida da minhoca é valida


== __Exemplos:__
>>> verificaVida Minhoca{posicaoMinhoca=Just (3,0), vidaMinhoca=Morta, jetpackMinhoca=100, escavadoraMinhoca=200, bazucaMinhoca=150, minaMinhoca=3, dinamiteMinhoca=1}
True
>>> verificaVida Minhoca{posicaoMinhoca=Just (3,0), vidaMinhoca=Viva 150, jetpackMinhoca=100, escavadoraMinhoca=200, bazucaMinhoca=150, minaMinhoca=3, dinamiteMinhoca=1}
False

-}
verificaVida :: Minhoca -> Bool
verificaVida m = case vida of
                    Morta -> True
                    Viva a -> a >= 0 && a <= 100
    where
        vida = vidaMinhoca m

-- | Verifica se uma minhoca esta viva
eMinhocaViva :: Minhoca -> Bool
eMinhocaViva m = case vidaMinhoca m of 
                    Morta -> False
                    Viva 0 -> False
                    Viva _ -> True
