module Main where

import Labs2025
import Tarefa1
import Magic

-- * Comandos para testar (rodar na cmd - bash - na pasta do ficheiro)
-- * cabal clean && rm -rf t1-feedback.tix
-- * cabal run --enable-coverage t1-feedback 
-- * ./runhpc.sh t1-feedback



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
    posicaoMinhoca = Just(5,7),
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
  { posicaoDisparo = (3,0)
  , direcaoDisparo = Norte
  , tipoDisparo = Dinamite
  , tempoDisparo = Just 2
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


minhoca7 = Minhoca{
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
  , minhocasEstado = [minhoca7]
  }

-- * ESTADO INVALIDO (donos repetidos)



estadoInvalidoDonosIguais = Estado
    { mapaEstado =
        [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Terra,Terra,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Terra,Terra,Pedra,Pedra,Agua,Agua,Agua]
        ]
    , objetosEstado =
        [Disparo {posicaoDisparo = (1,4), direcaoDisparo = Norte, tipoDisparo = Mina, tempoDisparo = Just 2, donoDisparo = 0}
        ,Disparo {posicaoDisparo = (2,4), direcaoDisparo = Norte, tipoDisparo = Dinamite, tempoDisparo = Just 2, donoDisparo = 1}
        ,Barril {posicaoBarril = (3,2), explodeBarril = False}
        ,Disparo {posicaoDisparo = (0,0), direcaoDisparo = Norte, tipoDisparo = Bazuca, tempoDisparo = Nothing, donoDisparo = 0}
        ,Disparo {posicaoDisparo = (1,1), direcaoDisparo = Este, tipoDisparo = Bazuca, tempoDisparo = Nothing, donoDisparo = 0}
        ]
    , minhocasEstado =
        [Minhoca {posicaoMinhoca = Just (5,7), vidaMinhoca = Morta, jetpackMinhoca = 100, escavadoraMinhoca = 200, bazucaMinhoca = 150, minaMinhoca = 3, dinamiteMinhoca = 1}
        ,Minhoca {posicaoMinhoca = Just (3,1), vidaMinhoca = Viva 0, jetpackMinhoca = 100, escavadoraMinhoca = 200, bazucaMinhoca = 150, minaMinhoca = 3, dinamiteMinhoca = 1}
        ]
    }

-- * ESTADO VALIDO 2


minhoca6 = Minhoca{
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
  , objetosEstado  = objetos ++ [mina, dina]
  , minhocasEstado = [minhoca6]
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


-- * ESTADO VALIDO 3

estadoValido3 = Estado
    { mapaEstado =
        [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Terra,Terra,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Terra,Terra,Pedra,Pedra,Agua,Agua,Agua]
        ]
    , objetosEstado =
        [Disparo {posicaoDisparo = (3,1), direcaoDisparo = Norte, tipoDisparo = Mina, tempoDisparo = Just 2, donoDisparo = 0}
        ,Disparo {posicaoDisparo = (2,4), direcaoDisparo = Norte, tipoDisparo = Dinamite, tempoDisparo = Just 2, donoDisparo = 1}
        ,Barril {posicaoBarril = (3,2), explodeBarril = False}
        ]
    , minhocasEstado =
        [Minhoca {posicaoMinhoca = Just (3,0), vidaMinhoca = Morta, jetpackMinhoca = 100, escavadoraMinhoca = 200, bazucaMinhoca = 150, minaMinhoca = 3, dinamiteMinhoca = 1}
        ,Minhoca {posicaoMinhoca = Just (3,1), vidaMinhoca = Viva 0, jetpackMinhoca = 100, escavadoraMinhoca = 200, bazucaMinhoca = 150, minaMinhoca = 3, dinamiteMinhoca = 1}
        ]
    }

-- * ESTADO INVALIDO (minhoca overlap mapa)

estadoInvalido3 = Estado
    { mapaEstado =
        [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Terra,Terra,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Terra,Terra,Pedra,Pedra,Agua,Agua,Agua]
        ]
    , objetosEstado =
        [Disparo {posicaoDisparo = (3,3), direcaoDisparo = Norte, tipoDisparo = Mina, tempoDisparo = Just 2, donoDisparo = 0}
        ,Disparo {posicaoDisparo = (2,4), direcaoDisparo = Norte, tipoDisparo = Dinamite, tempoDisparo = Just 2, donoDisparo = 1}
        ,Barril {posicaoBarril = (3,2), explodeBarril = False}
        ]
    , minhocasEstado =
        [Minhoca {posicaoMinhoca = Just (3,0), vidaMinhoca = Morta, jetpackMinhoca = 100, escavadoraMinhoca = 200, bazucaMinhoca = 150, minaMinhoca = 3, dinamiteMinhoca = 1}
        ,Minhoca {posicaoMinhoca = Just (4,2), vidaMinhoca = Viva 0, jetpackMinhoca = 100, escavadoraMinhoca = 200, bazucaMinhoca = 150, minaMinhoca = 3, dinamiteMinhoca = 1}
        ]
    }

-- * ESTADO INVALIDO (BARRIL OVERLAP MAPA)

estadoInvalido4 = Estado
    { mapaEstado =
        [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Terra,Terra,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Terra,Terra,Pedra,Pedra,Agua,Agua,Agua]
        ]
    , objetosEstado =
        [Disparo {posicaoDisparo = (3,3), direcaoDisparo = Norte, tipoDisparo = Mina, tempoDisparo = Just 2, donoDisparo = 0}
        ,Disparo {posicaoDisparo = (2,4), direcaoDisparo = Norte, tipoDisparo = Dinamite, tempoDisparo = Just 2, donoDisparo = 1}
        ,Barril {posicaoBarril = (4,3), explodeBarril = False}
        ]
    , minhocasEstado =
        [Minhoca {posicaoMinhoca = Just (3,0), vidaMinhoca = Morta, jetpackMinhoca = 100, escavadoraMinhoca = 200, bazucaMinhoca = 150, minaMinhoca = 3, dinamiteMinhoca = 1}
        ,Minhoca {posicaoMinhoca = Just (3,2), vidaMinhoca = Viva 0, jetpackMinhoca = 100, escavadoraMinhoca = 200, bazucaMinhoca = 150, minaMinhoca = 3, dinamiteMinhoca = 1}
        ]
    }

-- * ESTADO INVALIDO MINHOCA VIVA EM AGUA

estadoInvalido5 = Estado
    { mapaEstado =
        [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Terra,Terra,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Terra,Terra,Pedra,Pedra,Agua,Agua,Agua]
        ]
    , objetosEstado =
        [Disparo {posicaoDisparo = (3,3), direcaoDisparo = Norte, tipoDisparo = Mina, tempoDisparo = Just 2, donoDisparo = 0}
        ,Disparo {posicaoDisparo = (2,4), direcaoDisparo = Norte, tipoDisparo = Dinamite, tempoDisparo = Just 2, donoDisparo = 1}
        ,Barril {posicaoBarril = (3,5), explodeBarril = False}
        ]
    , minhocasEstado =
        [Minhoca {posicaoMinhoca = Just (3,0), vidaMinhoca = Morta, jetpackMinhoca = 100, escavadoraMinhoca = 200, bazucaMinhoca = 150, minaMinhoca = 3, dinamiteMinhoca = 1}
        ,Minhoca {posicaoMinhoca = Just (5,7), vidaMinhoca = Viva 0, jetpackMinhoca = 100, escavadoraMinhoca = 200, bazucaMinhoca = 150, minaMinhoca = 3, dinamiteMinhoca = 1}
        ]
    }


-- * ESTADO INVALIDO (TEMPO E POSICAO)

estadoInvalido6 = Estado
    { mapaEstado =
        [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Terra,Terra,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Terra,Terra,Pedra,Pedra,Agua,Agua,Agua]
        ]
    , objetosEstado =
        [Disparo {posicaoDisparo = (3,24), direcaoDisparo = Norte, tipoDisparo = Mina, tempoDisparo = Just 2, donoDisparo = 0}
        ,Disparo {posicaoDisparo = (2,4), direcaoDisparo = Norte, tipoDisparo = Dinamite, tempoDisparo = Just 23, donoDisparo = 1}
        ,Barril {posicaoBarril = (3,5), explodeBarril = False}
        ]
    , minhocasEstado =
        [Minhoca {posicaoMinhoca = Just (3,0), vidaMinhoca = Morta, jetpackMinhoca = 100, escavadoraMinhoca = 200, bazucaMinhoca = 150, minaMinhoca = 3, dinamiteMinhoca = 1}
        ,Minhoca {posicaoMinhoca = Just (5,7), vidaMinhoca = Viva 0, jetpackMinhoca = 100, escavadoraMinhoca = 200, bazucaMinhoca = 150, minaMinhoca = 3, dinamiteMinhoca = 1}
        ]
    }


-- * ESTADO INVALIDO BALAS TEMPO NULO

estadoBalaNulo = Estado
    { mapaEstado =
        [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Terra,Terra,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Terra,Terra,Pedra,Pedra,Agua,Agua,Agua]
        ]
    , objetosEstado =
        [Disparo {posicaoDisparo = (3,2), direcaoDisparo = Norte, tipoDisparo = Bazuca, tempoDisparo = Nothing, donoDisparo = 0},Disparo {posicaoDisparo = (3,2), direcaoDisparo = Norte, tipoDisparo = Mina, tempoDisparo = Nothing, donoDisparo = 0},Disparo {posicaoDisparo = (3,2), direcaoDisparo = Norte, tipoDisparo = Dinamite, tempoDisparo = Nothing, donoDisparo = 0}
        , Disparo {posicaoDisparo = (3,2), direcaoDisparo = Norte, tipoDisparo = Escavadora, tempoDisparo = Nothing, donoDisparo = 0}, Disparo {posicaoDisparo = (3,2), direcaoDisparo = Norte, tipoDisparo = Jetpack, tempoDisparo = Nothing, donoDisparo = 0}, Disparo {posicaoDisparo = (3,2), direcaoDisparo = Norte, tipoDisparo = Bazuca, tempoDisparo = Just 0, donoDisparo = 0}]
    , minhocasEstado =
        [Minhoca {posicaoMinhoca = Just (15,7), vidaMinhoca = Morta, jetpackMinhoca = 100, escavadoraMinhoca = 200, bazucaMinhoca = 150, minaMinhoca = 3, dinamiteMinhoca = 1}
        ,Minhoca {posicaoMinhoca = Nothing, vidaMinhoca = Morta, jetpackMinhoca = 100, escavadoraMinhoca = 200, bazucaMinhoca = 150, minaMinhoca = 3, dinamiteMinhoca = 1}
        
        ]
    }

-- * MINHOCAS E VAZIAS

estadoVazio = Estado
    { mapaEstado =
        []
    , objetosEstado =[]
    , minhocasEstado = []
    }



estadosParaTestar = [estado1, estadoInvalidoMapa,estadoInvalidoDonosIguais, estadoInvalidoMapaVazio, estadoInvalidoMinhocas, estadoInvalidoMinhocassss, estadoInvalidoObjetos, estadoValido2, estadoInvalido3, estadoInvalido4, estadoInvalido5, estadoInvalido6,estadoValido3, estadoBalaNulo,estadoVazio]

-- | Definir aqui os testes do grupo para a Tarefa 1
testesTarefa1 :: [Estado]
testesTarefa1 = estadosParaTestar

dataTarefa1 :: IO TaskData
dataTarefa1 = do
    let ins = testesTarefa1
    outs <- mapM (runTest . validaEstado) ins
    return $ T1 ins outs

main :: IO ()
main = runFeedback =<< dataTarefa1
