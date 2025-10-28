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

-- | Atualia a quantidade de munições disponíveis de uma minhoca para uma dada arma.
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
ePosicaoMapaLivre (x,y) [] = False
ePosicaoMapaLivre (0,y) (h:t) = encontraLinhaMapa y h
ePosicaoMapaLivre (x,y) (h:t) = ePosicaoMapaLivre (x-1,y) t

-- Auxiliares 
encontraLinhaMapa :: Int -> [Terreno] -> Bool
encontraLinhaMapa i [] = False
encontraLinhaMapa 0 (h:t) = not (eTerrenoOpaco h)
encontraLinhaMapa i (h:t) = encontraLinhaMapa (i-1) t 
-- *! SO esta livre se terreno opaco +e falso,
    
-- ---------------------

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



--Auxiliares -----------------

existeMinhoca :: Posicao -> [Minhoca] -> Bool
existeMinhoca pos [] = False
existeMinhoca (x,y) (h:t) = let pos = posicaoMinhoca h
                            in if pos == Just (x,y) then True else existeMinhoca (x,y) t
                       
existeBarril :: Posicao -> [Objeto] -> Bool
existeBarril pos [] = False
existeBarril pos (h:t) =
  let p = posicaoObjeto h
  in if (ehDisparo h == False)
       then if p == pos
              then True
              else existeBarril pos t
        else existeBarril pos t

-- * Verifica se o objeto fornecido é um disparo se não, é Barril

ehDisparo :: Objeto -> Bool
ehDisparo d@Disparo{} = True
ehDisparo _ = False

posicaoObjeto :: Objeto -> Posicao
posicaoObjeto d@(Disparo {})  = posicaoDisparo d
posicaoObjeto b@(Barril {}) = posicaoBarril b


-- -----------------


-- | Verifica se numa lista de objetos já existe um disparo feito para uma dada arma por uma dada minhoca.
minhocaTemDisparo :: TipoArma -> NumMinhoca -> [Objeto] -> Bool
minhocaTemDisparo _ _ [] = False 
minhocaTemDisparo tipo indiceMinhoca (h:ls) = if (i == indiceMinhoca && t == tipo) then True else minhocaTemDisparo tipo indiceMinhoca ls
    where
        i = donoDisparo h
        t = tipoDisparo h



-- | Destrói uma dada posição no mapa (tipicamente como consequência de uma explosão).
--
-- __NB__: Só terrenos @Terra@ pode ser destruídos.
-- Assumo que seja destruir e substituir por Ar
destroiPosicao :: Posicao -> Mapa -> Mapa
destroiPosicao pos mapa = atualizaPosicaoMatriz pos Ar mapa

-- Adiciona um novo objeto a um estado.
--
-- __NB__: A posição onde é inserido não é relevante.
adicionaObjeto :: Objeto -> Estado -> Estado
adicionaObjeto obj estado = estado {objetosEstado = obj : objetosEstado estado}

