module Main where

import Labs2025
import Tarefa3
import Magic


-- * Comandos para testar (rodar na cmd - bash - na pasta do ficheiro)
-- * cabal clean && rm -rf t3-feedback.tix
-- * cabal run --enable-coverage t3-feedback 
-- * ./runhpc.sh t3-feedback




mapaValido = [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Terra,Pedra,Agua,Agua,Agua,Agua,Agua]
        ,[Terra,Terra,Terra,Terra,Pedra,Pedra,Agua,Agua,Agua,Agua]
        ,[Terra,Terra,Terra,Terra,Terra,Pedra,Pedra,Pedra,Agua,Agua]]


-- * OBJETOS

barril1 = Barril {posicaoBarril = (2,0), explodeBarril = True}
barril2 = Barril {posicaoBarril = (0,0), explodeBarril = False}

bazuca1 = Disparo {posicaoDisparo = (0,5), direcaoDisparo = Este, tipoDisparo = Bazuca, tempoDisparo = Nothing, donoDisparo = 0}
bazuca2 = Disparo {posicaoDisparo = (2,3), direcaoDisparo = Sul, tipoDisparo = Bazuca, tempoDisparo = Nothing, donoDisparo = 0}
        

mina1 = Disparo {posicaoDisparo = (2,5), direcaoDisparo = Norte, tipoDisparo = Mina, tempoDisparo = Just 2, donoDisparo = 0}
mina2 = Disparo {posicaoDisparo = (3,5), direcaoDisparo = Sul, tipoDisparo = Mina, tempoDisparo = Just 2, donoDisparo = 0}

dinamite1 = Disparo {posicaoDisparo = (2,3), direcaoDisparo = Nordeste, tipoDisparo = Dinamite, tempoDisparo = Just 3, donoDisparo = 0}
dinamite2 = Disparo {posicaoDisparo = (2,7), direcaoDisparo = Sul, tipoDisparo = Dinamite, tempoDisparo = Just 3, donoDisparo = 0}
dinamite3 = Disparo {posicaoDisparo = (4,9), direcaoDisparo = Nordeste, tipoDisparo = Dinamite, tempoDisparo = Just 3, donoDisparo = 0}
               

-- * MINHOCAS

minhoca1 = Minhoca {posicaoMinhoca = Just (2,1), vidaMinhoca = Viva 100, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}
minhoca2 = Minhoca {posicaoMinhoca = Just (1,8), vidaMinhoca = Viva 100, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}
minhoca3 = Minhoca {posicaoMinhoca = Just (3,8), vidaMinhoca = Morta, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}
minhoca4 = Minhoca {posicaoMinhoca = Just (1,3), vidaMinhoca = Viva 0, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}
        
-- * ESTADOS

estado1 = Estado {mapaEstado = mapaValido, objetosEstado = [barril1, bazuca1, mina1, dinamite1],minhocasEstado = [minhoca1,minhoca2]}
estado2 = Estado {mapaEstado = mapaValido, objetosEstado = [barril2, bazuca2, mina2, dinamite2],minhocasEstado = [minhoca1,minhoca2]}
estado3 = Estado {mapaEstado = mapaValido, objetosEstado = [barril1, bazuca1, mina1, dinamite1],minhocasEstado = [minhoca1,minhoca3]}
estado4 = Estado {mapaEstado = mapaValido, objetosEstado = [barril2, bazuca2, mina2, dinamite2],minhocasEstado = [minhoca1,minhoca3]}
estado5 = Estado
    { mapaEstado =
        [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Terra,Pedra,Agua,Agua,Agua,Agua,Pedra]
        ,[Terra,Terra,Terra,Terra,Pedra,Pedra,Agua,Agua,Agua,Agua]
        ,[Terra,Terra,Terra,Terra,Terra,Pedra,Pedra,Pedra,Agua,Agua]
        ]
    , objetosEstado =
        [Disparo {posicaoDisparo = (2,0), direcaoDisparo = Sul, tipoDisparo = Bazuca, tempoDisparo = Nothing, donoDisparo = 0}
        ,Disparo {posicaoDisparo = (3,2), direcaoDisparo = Sul, tipoDisparo = Bazuca, tempoDisparo = Nothing, donoDisparo = 1}
        ]
    , minhocasEstado =
        [Minhoca {posicaoMinhoca = Just (2,8), vidaMinhoca = Viva 100, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}
        ,Minhoca {posicaoMinhoca = Just (2,4), vidaMinhoca = Viva 100, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}
        ]
    }
estado6 = Estado
    { mapaEstado =
        [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Terra,Pedra,Agua,Agua,Agua,Agua,Agua]
        ,[Terra,Terra,Terra,Terra,Pedra,Pedra,Agua,Agua,Agua,Agua]
        ,[Terra,Terra,Terra,Terra,Terra,Pedra,Pedra,Pedra,Agua,Agua]
        ]
    , objetosEstado =
        [Barril {posicaoBarril = (3,5), explodeBarril = False}
        ,Disparo {posicaoDisparo = (0,2), direcaoDisparo = Norte, tipoDisparo = Bazuca, tempoDisparo = Nothing, donoDisparo = 1}
        ,Disparo {posicaoDisparo = (2,0), direcaoDisparo = Sul, tipoDisparo = Dinamite, tempoDisparo = Just 4, donoDisparo = 0}
        ]
    , minhocasEstado =
        [Minhoca {posicaoMinhoca = Just (2,8), vidaMinhoca = Viva 100, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}
        ,Minhoca {posicaoMinhoca = Just (2,4), vidaMinhoca = Viva 100, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}
        ]
    }
estado7 = Estado
    { mapaEstado =
        [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Terra,Pedra,Agua,Agua,Agua,Agua,Agua]
        ,[Terra,Terra,Terra,Terra,Pedra,Pedra,Agua,Agua,Agua,Agua]
        ,[Terra,Terra,Terra,Terra,Terra,Pedra,Pedra,Pedra,Agua,Agua]
        ]
    , objetosEstado =
        [Barril {posicaoBarril = (2,3), explodeBarril = False}
        ,Disparo {posicaoDisparo = (2,2), direcaoDisparo = Este, tipoDisparo = Bazuca, tempoDisparo = Nothing, donoDisparo = 0}
        ]
    , minhocasEstado =
        [Minhoca {posicaoMinhoca = Just (2,4), vidaMinhoca = Viva 100, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}
        ]
    }
estado8 = Estado
    { mapaEstado =
        [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Terra,Pedra,Agua,Agua,Agua,Agua,Agua]
        ,[Terra,Terra,Terra,Terra,Pedra,Pedra,Agua,Agua,Agua,Agua]
        ,[Terra,Terra,Terra,Terra,Terra,Pedra,Pedra,Pedra,Agua,Agua]
        ]
    , objetosEstado =
        [Disparo {posicaoDisparo = (2,3), direcaoDisparo = Norte, tipoDisparo = Mina, tempoDisparo = Nothing, donoDisparo = 0}
        ]
    , minhocasEstado =
        [Minhoca {posicaoMinhoca = Just (2,4), vidaMinhoca = Viva 100, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}
        ,Minhoca {posicaoMinhoca = Just (2,3), vidaMinhoca = Viva 100, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}
        ]
    }
estado9 = Estado
    { mapaEstado =
        [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Terra,Pedra,Agua,Agua,Agua,Agua,Agua]
        ,[Terra,Terra,Terra,Terra,Pedra,Pedra,Agua,Agua,Agua,Agua]
        ,[Terra,Terra,Terra,Terra,Terra,Pedra,Pedra,Pedra,Agua,Agua]
        ]
    , objetosEstado =
        [Disparo {posicaoDisparo = (2,6), direcaoDisparo = Norte, tipoDisparo = Mina, tempoDisparo = Nothing, donoDisparo = 0}
        ]
    , minhocasEstado =
        [Minhoca {posicaoMinhoca = Just (2,3), vidaMinhoca = Viva 100, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}
        ,Minhoca {posicaoMinhoca = Just (1,3), vidaMinhoca = Viva 100, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}
        ]
    }
estado10 = Estado
    { mapaEstado =
        [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Terra,Pedra,Agua,Agua,Agua,Agua,Agua]
        ,[Terra,Terra,Terra,Terra,Pedra,Pedra,Agua,Agua,Agua,Agua]
        ,[Terra,Terra,Terra,Terra,Terra,Pedra,Pedra,Pedra,Agua,Agua]
        ]
    , objetosEstado =
        [Disparo {posicaoDisparo = (2,6), direcaoDisparo = Norte, tipoDisparo = Mina, tempoDisparo = Nothing, donoDisparo = 0}
        ]
    , minhocasEstado =
        [Minhoca {posicaoMinhoca = Just (1,3), vidaMinhoca = Viva 100, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}
        ,Minhoca {posicaoMinhoca = Just (2,3), vidaMinhoca = Viva 100, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}
        ]
    }
estado11 = Estado
    { mapaEstado =
        [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Terra,Pedra,Agua,Agua,Agua,Agua,Agua]
        ,[Terra,Terra,Terra,Terra,Pedra,Pedra,Agua,Agua,Agua,Agua]
        ,[Terra,Terra,Terra,Terra,Terra,Pedra,Pedra,Pedra,Agua,Agua]
        ]
    , objetosEstado =
        [Disparo {posicaoDisparo = (2,5), direcaoDisparo = Oeste, tipoDisparo = Bazuca, tempoDisparo = Nothing, donoDisparo = 0}
        ]
    , minhocasEstado =
        [Minhoca {posicaoMinhoca = Just (2,3), vidaMinhoca = Viva 100, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}
        ,Minhoca {posicaoMinhoca = Just (2,4), vidaMinhoca = Viva 100, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}
        ]
    }
estado12 = Estado
    { mapaEstado =
        [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Terra,Pedra,Agua,Agua,Agua,Agua,Agua]
        ,[Terra,Terra,Terra,Terra,Pedra,Pedra,Agua,Agua,Agua,Agua]
        ,[Terra,Terra,Terra,Terra,Terra,Pedra,Pedra,Pedra,Agua,Agua]
        ]
    , objetosEstado =
        [Disparo {posicaoDisparo = (2,4), direcaoDisparo = Oeste, tipoDisparo = Bazuca, tempoDisparo = Nothing, donoDisparo = 0}
        ]
    , minhocasEstado =
        [Minhoca {posicaoMinhoca = Just (2,3), vidaMinhoca = Viva 100, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}
        ,Minhoca {posicaoMinhoca = Just (2,4), vidaMinhoca = Viva 100, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}
        ]
    }
estado13 = Estado
    { mapaEstado =
        [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Terra,Pedra,Agua,Agua,Agua,Agua,Agua]
        ,[Terra,Terra,Terra,Terra,Pedra,Pedra,Agua,Agua,Agua,Agua]
        ,[Terra,Terra,Terra,Terra,Terra,Pedra,Pedra,Pedra,Agua,Agua]
        ]
    , objetosEstado =
        [Disparo {posicaoDisparo = (2,4), direcaoDisparo = Sul, tipoDisparo = Mina, tempoDisparo = Nothing, donoDisparo = 0}
        ]
    , minhocasEstado =
        [Minhoca {posicaoMinhoca = Just (2,3), vidaMinhoca = Viva 100, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}
        ,Minhoca {posicaoMinhoca = Just (2,4), vidaMinhoca = Viva 100, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}
        ]
    }
estado14 = Estado
    { mapaEstado =
        [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Terra,Pedra,Agua,Agua,Agua,Agua,Agua]
        ,[Terra,Terra,Terra,Terra,Pedra,Pedra,Agua,Agua,Agua,Agua]
        ,[Terra,Terra,Terra,Terra,Terra,Pedra,Pedra,Pedra,Agua,Agua]
        ]
    , objetosEstado =
        [Disparo {posicaoDisparo = (3,6), direcaoDisparo = Sul, tipoDisparo = Mina, tempoDisparo = Nothing, donoDisparo = 0}
        ]
    , minhocasEstado =
        [Minhoca {posicaoMinhoca = Just (2,3), vidaMinhoca = Viva 100, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}
        ,Minhoca {posicaoMinhoca = Just (2,4), vidaMinhoca = Viva 100, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}
        ]
    }
estado15 = Estado
    { mapaEstado =
        [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Terra,Pedra,Agua,Agua,Agua,Agua,Agua]
        ,[Terra,Terra,Terra,Terra,Pedra,Pedra,Agua,Agua,Agua,Agua]
        ,[Terra,Terra,Terra,Terra,Terra,Pedra,Pedra,Pedra,Agua,Agua]
        ]
    , objetosEstado =
        [Disparo {posicaoDisparo = (2,3), direcaoDisparo = Sul, tipoDisparo = Mina, tempoDisparo = Nothing, donoDisparo = 0}
        ]
    , minhocasEstado =
        [Minhoca {posicaoMinhoca = Just (2,3), vidaMinhoca = Viva 100, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}
        ,Minhoca {posicaoMinhoca = Just (2,4), vidaMinhoca = Viva 100, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}
        ]
    }
estado16 = Estado
    { mapaEstado =
        [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Terra,Pedra,Agua,Agua,Agua,Agua,Agua]
        ,[Terra,Terra,Terra,Terra,Pedra,Pedra,Agua,Agua,Agua,Agua]
        ,[Terra,Terra,Terra,Terra,Terra,Pedra,Pedra,Pedra,Agua,Agua]
        ]
    , objetosEstado =
        [Barril {posicaoBarril = (2,0), explodeBarril = True}]
    , minhocasEstado =
        [Minhoca {posicaoMinhoca = Just (2,3), vidaMinhoca = Viva 100, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}
        ,Minhoca {posicaoMinhoca = Just (2,4), vidaMinhoca = Viva 100, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}
        ]
    }
estado17 = Estado{objetosEstado = []
        , minhocasEstado = [Minhoca {posicaoMinhoca = Just (4,4), vidaMinhoca = Morta, jetpackMinhoca = 1, 
        escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 62},
                Minhoca {posicaoMinhoca = Just (3,4), vidaMinhoca = Viva 60, jetpackMinhoca = 1, 
        escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 62}] 
        ,mapaEstado = [[Ar,Ar,Ar,Ar,Ar],
                        [Ar,Ar,Ar,Ar,Ar],
                        [Ar,Terra,Ar,Ar,Ar],
                        [Terra,Terra,Terra,Ar,Ar],
                        [Pedra,Pedra,Pedra,Ar,Agua]]}
estado18 = Estado{objetosEstado = []
        , minhocasEstado = [Minhoca {posicaoMinhoca = Just (3,4), vidaMinhoca = Morta, jetpackMinhoca = 1, 
        escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 62},
                Minhoca {posicaoMinhoca = Just (4,4), vidaMinhoca = Morta, jetpackMinhoca = 1, 
        escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 62}] 
        ,mapaEstado = [[Ar,Ar,Ar,Ar,Ar],
                        [Ar,Ar,Ar,Ar,Ar],
                        [Ar,Terra,Ar,Ar,Ar],
                        [Terra,Terra,Terra,Ar,Ar],
                        [Pedra,Pedra,Pedra,Ar,Agua]]}
estado19 = Estado
    { mapaEstado =
        [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra]
        ]
    , objetosEstado =
        []
    , minhocasEstado =
        [Minhoca {posicaoMinhoca = Just (0,3), vidaMinhoca = Viva 100, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}
        ,Minhoca {posicaoMinhoca = Just (1,3), vidaMinhoca = Viva 100, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}
        ]
    }
estado20 = Estado
    { mapaEstado =
        [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra]
        ]
    , objetosEstado =
        []
    , minhocasEstado =
        [Minhoca {posicaoMinhoca = Just (0,3), vidaMinhoca = Viva 100, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}
        ,Minhoca {posicaoMinhoca = Just (1,3), vidaMinhoca = Morta, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}
        ]
    }
estado21 = Estado
    { mapaEstado =
        [[Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Agua,Agua,Agua]
        ,[Terra,Terra,Terra,Agua,Agua,Agua]
        ,[Terra,Terra,Terra,Agua,Agua,Agua]
        ]
    , objetosEstado =
        [Barril {posicaoBarril = (4,4), explodeBarril = False}
        ]
    , minhocasEstado =
        [Minhoca {posicaoMinhoca = Just (2,0), vidaMinhoca = Viva 100, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}
        ]
    }
estado22 = Estado
    { mapaEstado =
        [[Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Agua,Agua,Agua]
        ,[Terra,Terra,Terra,Agua,Agua,Agua]
        ,[Terra,Terra,Terra,Terra,Terra,Terra]
        ]
    , objetosEstado =
        [Barril {posicaoBarril = (4,4), explodeBarril = False}
        ]
    , minhocasEstado =
        [Minhoca {posicaoMinhoca = Just (2,1), vidaMinhoca = Viva 100, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}
        ]
    }
estado23 = Estado
    { mapaEstado =
        [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra]
        ]
    , objetosEstado =
        [Barril {posicaoBarril = (1,3), explodeBarril = True}
        ]
    , minhocasEstado =
        [Minhoca {posicaoMinhoca = Just (2,3), vidaMinhoca = Viva 100, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}
        ]
    }
estado24 = Estado
    { mapaEstado =
        [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Terra,Agua,Agua,Agua]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Agua,Agua]
        ]
    , objetosEstado =
        [Disparo {posicaoDisparo = (3,7), direcaoDisparo = Sul, tipoDisparo = Mina, tempoDisparo = Nothing, donoDisparo = 0}
        ]
    , minhocasEstado =
        [Minhoca {posicaoMinhoca = Just (2,3), vidaMinhoca = Viva 100, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}
        ]
    }
estado25 = Estado
    { mapaEstado =
        [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Terra,Agua,Agua,Agua]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Agua,Agua]
        ]
    , objetosEstado =
        [Barril {posicaoBarril = (3,7), explodeBarril = True}
        ,Barril {posicaoBarril = (0,0), explodeBarril = False}
        ]
    , minhocasEstado =
        [Minhoca {posicaoMinhoca = Just (2,3), vidaMinhoca = Viva 100, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}
        ]
    }
estado26 = Estado
    { mapaEstado =
        [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Terra,Agua,Agua,Agua]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Agua,Agua]
        ]
    , objetosEstado =
        [Disparo {posicaoDisparo = (2,5), direcaoDisparo = Noroeste, tipoDisparo = Dinamite, tempoDisparo = Just 0, donoDisparo = 0}
        ]
    , minhocasEstado =
        [Minhoca {posicaoMinhoca = Just (2,3), vidaMinhoca = Viva 100, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}
        ]
    }
estado27 = Estado
    { mapaEstado =
        [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Terra,Agua,Agua,Agua]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Agua,Agua]
        ]
    , objetosEstado =
        [Disparo {posicaoDisparo = (2,4), direcaoDisparo = Sul, tipoDisparo = Bazuca, tempoDisparo = Nothing, donoDisparo = 0}
        ]
    , minhocasEstado =
        [Minhoca {posicaoMinhoca = Just (2,3), vidaMinhoca = Viva 100, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}
        ]
    }
estado28 = Estado
    { mapaEstado =
        [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Terra,Agua,Agua,Agua]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Agua,Agua]
        ]
    , objetosEstado =
        [Disparo {posicaoDisparo = (1,4), direcaoDisparo = Sul, tipoDisparo = Mina, tempoDisparo = Nothing, donoDisparo = 0}
        ]
    , minhocasEstado =
        [Minhoca {posicaoMinhoca = Just (2,3), vidaMinhoca = Viva 100, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}
        ]
    }
estado29 = Estado
    { mapaEstado =
        [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Terra,Agua,Agua,Agua]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Agua,Agua]
        ]
    , objetosEstado =
        [Disparo {posicaoDisparo = (1,4), direcaoDisparo = Sul, tipoDisparo = Mina, tempoDisparo = Just 2, donoDisparo = 0}
        ]
    , minhocasEstado =
        [Minhoca {posicaoMinhoca = Just (2,3), vidaMinhoca = Viva 100, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}
        ]
    }
estado30 = Estado
    { mapaEstado =
        [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Agua,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Terra,Agua,Agua,Agua]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Agua,Agua]
        ]
    , objetosEstado =
        [Disparo {posicaoDisparo = (2,4), direcaoDisparo = Sul, tipoDisparo = Mina, tempoDisparo = Just 2, donoDisparo = 0}
        ]
    , minhocasEstado =
        [Minhoca {posicaoMinhoca = Just (2,3), vidaMinhoca = Viva 100, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}
        ,Minhoca {posicaoMinhoca = Just (2,4), vidaMinhoca = Viva 100, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}
        ]
    }
estado31 = Estado
    { mapaEstado =
        [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Terra,Agua,Agua,Agua]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Agua,Agua]
        ]
    , objetosEstado =
        []
    , minhocasEstado =
        [Minhoca {posicaoMinhoca = Just (1,4), vidaMinhoca = Viva 0, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}
        ]
    }
estado32 = Estado
    { mapaEstado =
        [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Terra,Agua,Agua,Agua]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Agua,Agua]
        ]
    , objetosEstado =
        [Barril {posicaoBarril = (1,0), explodeBarril = True}
        ,Barril {posicaoBarril = (2,0), explodeBarril = False}
        ]
    , minhocasEstado =
        [Minhoca {posicaoMinhoca = Just (1,4), vidaMinhoca = Viva 0, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}
        ]
    }
estado33 = Estado
    { mapaEstado =
        [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Terra,Agua,Agua,Agua]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Agua,Agua]
        ]
    , objetosEstado =
        [Barril {posicaoBarril = (1,0), explodeBarril = True}
        ,Barril {posicaoBarril = (2,0), explodeBarril = False}
        ]
    , minhocasEstado =
        [Minhoca {posicaoMinhoca = Just (1,1), vidaMinhoca = Viva 20, jetpackMinhoca = 1, escavadoraMinhoca = 1, bazucaMinhoca = 1, minaMinhoca = 1, dinamiteMinhoca = 1}
        ]
    }


-- | Definir aqui os testes do grupo para a Tarefa 3
testesTarefa3 :: [Estado]
testesTarefa3 = [estado1,estado2,estado3,estado4,estado5,estado6,estado7,estado8, estado10, 
                estado11, estado12, estado13, estado14, estado15, estado16, estado17, estado18, 
                estado19, estado20, estado21, estado22, estado23, estado24, estado25, estado26, estado27,
                estado28, estado29, estado30, estado31, estado32, estado33]

dataTarefa3 :: IO TaskData
dataTarefa3 = do
    let ins = testesTarefa3
    outs <- mapM (runTest . avancaEstado) ins
    return $ T3 ins outs

main :: IO ()
main = runFeedback =<< dataTarefa3

-- TOdo justificar no haddock o estado20