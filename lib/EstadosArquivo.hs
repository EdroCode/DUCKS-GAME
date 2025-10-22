module EstadosArquivo where


-- dedicada ao teste de funçcões para melhor entendimento da linguagem / armazenar estados teste
type Posicao = (Int,Int)


encontraLista :: Int -> [a] -> Bool
encontraLista i [] = False
encontraLista i (h:t) |i == 0 = True 
                      |otherwise = encontraLista (i-1) t

encontraMatriz :: Posicao -> [[a]] -> Bool
encontraMatriz p [] = False
encontraMatriz (0,c) (h:t) = encontraLista c h
encontraMatriz (l,c) (h:t) = encontraMatriz (l-1,c) t

-- encontra (1,2) [(1,2),(1,2)]
-- encontra (0,2) [(1,2)]
-- encontraLista 2 [1,2]
-- encontra lista 1 [2]
-- encontra lista 0 [] -> False


substituirLista :: Int -> Int -> [Int] -> [Int]
substituirLista _ _ [] = []
substituirLista 0 x (h:t) = x : t
substituirLista i x (h:t) = h : substituirLista (i-1) x t


substituirMatriz :: Int -> (Int, Int) -> [[Int]] -> [[Int]] -- [[1,2,3], [4,5,6]]
substituirMatriz x pos [] = []
substituirMatriz x (0, c) (h:t) = substituirLista c x h : t
substituirMatriz x (l,c) (h:t) = h : substituirMatriz x (l - 1, c) t




-- * -----------------------------------------
-- * ESTADOS
-- * -----------------------------------------





m = [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
    ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
    ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
    ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
    ,[Terra,Terra,Terra,Terra,Terra,Ar,Ar,Ar,Ar,Ar]
    ,[Terra,Terra,Terra,Terra,Terra,Pedra,Pedra,Agua,Agua,Agua]
    ]

barrilTeste = Barril
  { posicaoBarril = (3,2)
  , explodeBarril = False
  }

disparo1 = Disparo
  { posicaoDisparo = (1,4)
  , direcaoDisparo = Norte
  , tipoDisparo = Mina
  , tempoDisparo = Just 2
  , donoDisparo = 0
}

disparo2 = Disparo
  { posicaoDisparo = (2,4)
  , direcaoDisparo = Norte
  , tipoDisparo = Dinamite
  , tempoDisparo = Just 2
  , donoDisparo = 1
}

minhoca1 = Minhoca{
    posicaoMinhoca = Just(3,0),
    vidaMinhoca = Morta,
    jetpackMinhoca = 100,
    escavadoraMinhoca = 200,
    bazucaMinhoca = 150,
    minaMinhoca = 3,
    dinamiteMinhoca = 1
    }

minhoca2 = Minhoca{
    posicaoMinhoca = Just(3,1),
    vidaMinhoca = Viva 0,
    jetpackMinhoca = 100,
    escavadoraMinhoca = 200,
    bazucaMinhoca = 150,
    minaMinhoca = 3,
    dinamiteMinhoca = 1
    }

minhocas = [minhoca1, minhoca2]
objetos = [disparo1, disparo2, barrilTeste]


estado1 = Estado
  { mapaEstado     = m
  , objetosEstado  = objetos
  , minhocasEstado = minhocas
  }

-- * ###################################

-- * ESTADO INVALIDO (por mapa)

mapaInvalido = [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
    ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
    ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
    ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
    ,[Terra,Terra,Terra,Terra,Terra,Ar,Ar,Ar,Ar,Ar]
    ,[Terra,Terra,Terra,Terra,Terra,Pedra,Pedra,Agua,Agua]
    ]

estadoInvalidoMapa = Estado
  { mapaEstado     = mapaInvalido
  , objetosEstado  = objetos
  , minhocasEstado = minhocas
  }

-- * ###################################

-- * ESTADO INVALIDO (por objetos)


disparo3 = Disparo
  { posicaoDisparo = (1,4)
  , direcaoDisparo = Norte
  , tipoDisparo = Mina
  , tempoDisparo = Just 24
  , donoDisparo = 0
}

estadoInvalidoObjetos = Estado
  { mapaEstado     = m
  , objetosEstado  = [disparo3]
  , minhocasEstado = minhocas
  }


-- * ###################################

-- * ESTADO INVALIDO (por minhocas)


minhoca3 = Minhoca{
    posicaoMinhoca = Just(3,0),
    vidaMinhoca = Morta,
    jetpackMinhoca = 400,
    escavadoraMinhoca = 200,
    bazucaMinhoca = 150,
    minaMinhoca = 3,
    dinamiteMinhoca = 1
    }


estadoInvalidoMinhocas = Estado
  { mapaEstado     = m
  , objetosEstado  = objetos
  , minhocasEstado = [minhoca3]
  }

-- * ESTADO INVALIDO (mapa vazio)


minhoca3 = Minhoca{
    posicaoMinhoca = Just(3,0),
    vidaMinhoca = Morta,
    jetpackMinhoca = 400,
    escavadoraMinhoca = 200,
    bazucaMinhoca = 150,
    minaMinhoca = 3,
    dinamiteMinhoca = 1
    }


estadoInvalidoMapaVazio = Estado
  { mapaEstado     = []
  , objetosEstado  = objetos
  , minhocasEstado = [minhoca3]
  }

-- * ESTADO VALIDO 2


minhoca3 = Minhoca{
    posicaoMinhoca = Just(3,0),
    vidaMinhoca = Morta,
    jetpackMinhoca = 400,
    escavadoraMinhoca = 200,
    bazucaMinhoca = 150,
    minaMinhoca = 3,
    dinamiteMinhoca = 1
    }

mina = Disparo
  { posicaoDisparo = (1,4)
  , direcaoDisparo = Norte
  , tipoDisparo = Mina
  , tempoDisparo = Just 2
  , donoDisparo = 0
}


dina = Disparo
  { posicaoDisparo = (2,4)
  , direcaoDisparo = Norte
  , tipoDisparo = Dinamite
  , tempoDisparo = Just 2
  , donoDisparo = 0
}

estadoValido2 = Estado
  { mapaEstado     = m
  , objetosEstado  = [objetos] ++ [mina, dina]
  , minhocasEstado = [minhoca3]
  }


-- * ESTADO INVALIDO POSICAO DUPLICADAS

minhoca4 = Minhoca{
    posicaoMinhoca = Just(3,0),
    vidaMinhoca = Morta,
    jetpackMinhoca = 400,
    escavadoraMinhoca = 200,
    bazucaMinhoca = 150,
    minaMinhoca = 3,
    dinamiteMinhoca = 1
    }

minhoca5 = Minhoca{
    posicaoMinhoca = Just(3,0),
    vidaMinhoca = Morta,
    jetpackMinhoca = 400,
    escavadoraMinhoca = 200,
    bazucaMinhoca = 150,
    minaMinhoca = 3,
    dinamiteMinhoca = 1
    }


estadoInvalidoMinhocassss = Estado
  { mapaEstado     = []
  , objetosEstado  = objetos
  , minhocasEstado = [minhoca5, minhoca4]
  }

estadosParaTestar = [estado1, estadoInvalidoMapa, estadoInvalidoMapaVazio, estadoInvalidoMinhocas, estadoInvalidoMinhocassss, estadoInvalidoObjetos, estadoValido2]