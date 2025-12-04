module Main where

import Labs2025
import Tarefa4
import Magic




testes :: [Estado]
testes = [Estado { mapaEstado =
        [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Ar,Pedra,Agua,Agua,Agua,Agua,Agua]
        ,[Terra,Terra,Terra,Terra,Pedra,Pedra,Agua,Agua,Agua,Agua]
        ,[Terra,Terra,Terra,Terra,Terra,Pedra,Pedra,Pedra,Agua,Agua]
        ]
    , objetosEstado =
        []
    , minhocasEstado =
        [Minhoca {posicaoMinhoca = Just (2,1), vidaMinhoca = Viva 70, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 0, minaMinhoca = 1, dinamiteMinhoca = 0}
        ,Minhoca {posicaoMinhoca = Just (1,4), vidaMinhoca = Viva 70, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 0, minaMinhoca = 1, dinamiteMinhoca = 0}
        ]
    }, Estado
    { mapaEstado =
        [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra]
        ]
    , objetosEstado =
        []
    , minhocasEstado =
        [Minhoca {posicaoMinhoca = Just (4,1), vidaMinhoca = Viva 100, jetpackMinhoca = 1, escavadoraMinhoca = 0, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 0}
        ,Minhoca {posicaoMinhoca = Just (4,9), vidaMinhoca = Viva 100, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}
        ]
    },Estado
    { mapaEstado =
        [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Ar,Terra,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Ar,Pedra,Agua,Agua,Agua,Agua,Agua]
        ,[Terra,Terra,Terra,Terra,Pedra,Pedra,Agua,Agua,Agua,Agua]
        ,[Terra,Terra,Terra,Terra,Terra,Pedra,Pedra,Pedra,Agua,Agua]
        ]
    , objetosEstado =
        []
    , minhocasEstado =
        [Minhoca {posicaoMinhoca = Just (2,1), vidaMinhoca = Viva 70, jetpackMinhoca = 1, escavadoraMinhoca = 13, bazucaMinhoca = 0, minaMinhoca = 1, dinamiteMinhoca = 0}
        ,Minhoca {posicaoMinhoca = Just (2,4), vidaMinhoca = Viva 70, jetpackMinhoca = 1, escavadoraMinhoca = 13, bazucaMinhoca = 0, minaMinhoca = 1, dinamiteMinhoca = 0}
        ]
    },Estado
    { mapaEstado =
        [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Ar,Terra,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Ar,Pedra,Agua,Agua,Agua,Agua,Agua]
        ,[Terra,Terra,Terra,Terra,Pedra,Pedra,Agua,Agua,Agua,Agua]
        ,[Terra,Terra,Terra,Terra,Terra,Pedra,Pedra,Pedra,Agua,Agua]
        ]
    , objetosEstado =
        []
    , minhocasEstado =
        [Minhoca {posicaoMinhoca = Nothing, vidaMinhoca = Viva 70, jetpackMinhoca = 1, escavadoraMinhoca = 100, bazucaMinhoca = 0, minaMinhoca = 1, dinamiteMinhoca = 0}
        ,Minhoca {posicaoMinhoca = Nothing, vidaMinhoca = Viva 70, jetpackMinhoca = 1, escavadoraMinhoca = 100, bazucaMinhoca = 0, minaMinhoca = 1, dinamiteMinhoca = 0}
        ]
    },
    Estado
    { mapaEstado =
        [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Ar,Terra,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Terra,Pedra,Agua,Agua,Agua,Agua,Agua]
        ,[Terra,Terra,Terra,Terra,Pedra,Pedra,Agua,Agua,Agua,Agua]
        ,[Terra,Terra,Terra,Terra,Terra,Pedra,Pedra,Pedra,Agua,Agua]
        ]
    , objetosEstado =
        [Disparo {posicaoDisparo = (2,3), direcaoDisparo = Norte, tipoDisparo = Mina, tempoDisparo = Nothing, donoDisparo = 1}
        ]
    , minhocasEstado =
        [Minhoca {posicaoMinhoca = Just (1,2), vidaMinhoca = Viva 70, jetpackMinhoca = 1, escavadoraMinhoca = 10, bazucaMinhoca = 0, minaMinhoca = 1, dinamiteMinhoca = 0}
        ,Minhoca {posicaoMinhoca = Just (2,4), vidaMinhoca = Viva 70, jetpackMinhoca = 1, escavadoraMinhoca = 13, bazucaMinhoca = 0, minaMinhoca = 1, dinamiteMinhoca = 0}
        ]
    },
    Estado
    { mapaEstado =
        [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Terra,Terra,Ar,Ar,Ar,Terra,Terra,Terra,Terra]
        ,[Ar,Ar,Ar,Ar,Pedra,Agua,Agua,Agua,Agua,Agua,Pedra,Terra,Terra,Terra,Ar,Ar,Terra,Terra,Terra,Terra]
        ,[Ar,Ar,Terra,Ar,Pedra,Pedra,Agua,Agua,Agua,Agua,Pedra,Terra,Terra,Terra,Ar,Ar,Terra,Terra,Terra,Terra]
        ,[Terra,Terra,Terra,Terra,Terra,Pedra,Pedra,Pedra,Agua,Agua,Pedra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Terra]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Terra,Terra]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra]
        ]
    , objetosEstado =
        [Barril {posicaoBarril = (1,16), explodeBarril = False}
        ,Disparo {posicaoDisparo = (0,17), tipoDisparo = Dinamite, direcaoDisparo = Norte, donoDisparo = 2, tempoDisparo = Just 5}
        ]
    , minhocasEstado =
        [Minhoca {posicaoMinhoca = Just (2,1), vidaMinhoca = Viva 70, jetpackMinhoca = 1, escavadoraMinhoca = 6, bazucaMinhoca = 0, minaMinhoca = 1, dinamiteMinhoca = 0}]
    },Estado { mapaEstado =
        [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Terra,Pedra,Agua,Agua,Agua,Agua,Agua]
        ,[Terra,Terra,Terra,Terra,Pedra,Pedra,Agua,Agua,Agua,Agua]
        ,[Terra,Terra,Terra,Terra,Terra,Pedra,Pedra,Pedra,Agua,Agua]
        ]
    , objetosEstado =
        [Barril {posicaoBarril = (2,3), explodeBarril = False}
        ]
    , minhocasEstado =
        [Minhoca {posicaoMinhoca = Just (2,8), vidaMinhoca = Viva 100, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}
        ,Minhoca {posicaoMinhoca = Just (2,4), vidaMinhoca = Viva 100, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}
        ]
    },
    Estado
    { mapaEstado =
        [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Terra,Terra,Ar,Ar,Ar,Terra,Terra,Terra,Terra]
        ,[Ar,Ar,Ar,Ar,Pedra,Agua,Agua,Agua,Agua,Agua,Pedra,Terra,Terra,Terra,Ar,Ar,Terra,Terra,Terra,Terra]
        ,[Ar,Ar,Terra,Ar,Pedra,Pedra,Agua,Agua,Agua,Agua,Pedra,Terra,Terra,Terra,Ar,Ar,Terra,Terra,Terra,Terra]
        ,[Terra,Terra,Terra,Terra,Terra,Pedra,Pedra,Pedra,Agua,Agua,Pedra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Terra]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Terra,Terra]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra]
        ]
    , objetosEstado =
        [Barril {posicaoBarril = (1,16), explodeBarril = False}
        ]
    , minhocasEstado =
        [Minhoca {posicaoMinhoca = Just (4,3), vidaMinhoca = Viva 70, jetpackMinhoca = 1, escavadoraMinhoca = 6, bazucaMinhoca = 0, minaMinhoca = 1, dinamiteMinhoca = 0}
        ,Minhoca {posicaoMinhoca = Just (4,0), vidaMinhoca = Viva 60, jetpackMinhoca = 1, escavadoraMinhoca = 9, bazucaMinhoca = 0, minaMinhoca = 1, dinamiteMinhoca = 0}
        ,Minhoca {posicaoMinhoca = Just (7,6), vidaMinhoca = Viva 100, jetpackMinhoca = 1, escavadoraMinhoca = 6, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 2}
        ,Minhoca {posicaoMinhoca = Just (1,11), vidaMinhoca = Viva 100, jetpackMinhoca = 1, escavadoraMinhoca = 6, bazucaMinhoca = 4, minaMinhoca = 1, dinamiteMinhoca = 3}
        ]
    },
    Estado
    { mapaEstado =
        [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Pedra,Pedra,Ar,Pedra,Agua,Agua,Agua,Agua,Agua]
        ,[Terra,Terra,Terra,Terra,Pedra,Pedra,Agua,Agua,Agua,Agua]
        ,[Terra,Terra,Terra,Terra,Terra,Pedra,Pedra,Pedra,Agua,Agua]
        ]
    , objetosEstado =
        []
    , minhocasEstado =
        [Minhoca {posicaoMinhoca = Just (1,0), vidaMinhoca = Viva 70, jetpackMinhoca = 1, escavadoraMinhoca = 0, bazucaMinhoca = 0, minaMinhoca = 1, dinamiteMinhoca = 0}
        ]
    },
    Estado
    { mapaEstado =
        [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Terra,Terra,Terra,Terra,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Agua,Agua,Agua,Agua,Agua]
        ,[Terra,Terra,Terra,Terra,Pedra,Pedra,Agua,Agua,Agua,Agua]
        ,[Terra,Terra,Terra,Terra,Terra,Pedra,Pedra,Pedra,Agua,Agua]
        ]
    , objetosEstado =
        []
    , minhocasEstado =
        [Minhoca {posicaoMinhoca = Just (3,3), vidaMinhoca = Viva 70, jetpackMinhoca = 1, escavadoraMinhoca = 0, bazucaMinhoca = 0, minaMinhoca = 1, dinamiteMinhoca = 1}
        ]
    },
    Estado
    { mapaEstado =
        [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Terra,Terra,Terra,Terra,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Terra,Ar,Terra,Agua,Agua,Agua,Agua,Agua]
        ,[Terra,Terra,Terra,Terra,Pedra,Pedra,Agua,Agua,Agua,Agua]
        ,[Terra,Terra,Terra,Terra,Terra,Pedra,Pedra,Pedra,Agua,Agua]
        ]
    , objetosEstado =
        []
    , minhocasEstado =
        [Minhoca {posicaoMinhoca = Just (3,3), vidaMinhoca = Viva 70, jetpackMinhoca = 1, escavadoraMinhoca = 0, bazucaMinhoca = 0, minaMinhoca = 1, dinamiteMinhoca = 2}
        ]
    },
    Estado
    { mapaEstado =
        [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Terra,Ar,Ar,Pedra,Ar,Agua,Agua,Agua,Agua]
        ,[Ar,Terra,Ar,Ar,Pedra,Ar,Agua,Agua,Agua,Agua]
        ,[Agua,Terra,Agua,Ar,Terra,Agua,Pedra,Pedra,Agua,Agua]
        ]
    , objetosEstado =
        []
    , minhocasEstado =
        [Minhoca {posicaoMinhoca = Just (2,1), vidaMinhoca = Viva 70, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 0, minaMinhoca = 1, dinamiteMinhoca = 0}
        ,Minhoca {posicaoMinhoca = Just (1,4), vidaMinhoca = Viva 70, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 0, minaMinhoca = 1, dinamiteMinhoca = 0}
        ]
    }]







-- | Definir aqui os testes do grupo para a Tarefa 4
testesTarefa4 :: [Estado]
testesTarefa4 = testes

dataTarefa4 :: IO TaskData
dataTarefa4 = do
    let ins = testesTarefa4
    outs <- mapM (runTest . tatica) ins
    return $ T4 ins outs

main :: IO ()
main = runFeedback =<< dataTarefa4