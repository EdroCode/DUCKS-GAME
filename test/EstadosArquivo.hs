module EstadosArquivo where

import Labs2025
import Tarefa0_2025
import Tarefa0_geral

-- dedicada ao teste de funçcões para melhor entendimento da linguagem / armazenar estados teste
type Posicao = (Int,Int)


-- * Aula

--getMunicoesMinhoca :: Minhoca -> Int
--getMunicoesMinhoca m = bazucaMinhoca m

--incrementMunBazuca :: Minhoca -> Int -> Minhoca
--incrementMunBazuca m i = m Estado{bazucaMinhoca = bazucaMinhoca m + i}
-- -incrementMunBazuca m@Minhoca Estado{bazucaMinhoca = bazucas} i - m Estado{bazucaMinhoca = bazucas}

-- * ------------------------------------------------------------


-- * -----------------------------------------
-- * ESTADOS
-- * -----------------------------------------

-- * -----------------------------------------
-- * MAPAS
-- * -----------------------------------------

mapaOk = [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
    ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
    ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
    ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
    ,[Terra,Terra,Terra,Terra,Terra,Ar,Ar,Ar,Ar,Ar]
    ,[Terra,Terra,Terra,Terra,Terra,Pedra,Pedra,Agua,Agua,Agua]
    ]

mapaVazio = []

mapaInvalido1 = [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
    ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
    ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
    ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
    ,[Terra,Terra,Terra,Terra,Terra,Ar,Ar,Ar,Ar,Ar]
    ,[Terra,Terra,Terra,Terra,Terra,Pedra,Pedra,Agua,Agua,Agua,Agua]
    ]


mapaInvalido2 = [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar] -- * Agora ta valido?
    ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
    ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
    ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
    ,[Terra,Terra,Terra,Terra,Terra,Ar,Ar,Ar,Ar,Ar]
    ,[Terra,Terra,Terra,Terra,Terra,Pedra,Pedra,Agua,Agua,Agua,Agua]
    ]

-- * -----------------------------------------
-- * OBJETOS
-- * -----------------------------------------

-- DISPAROS VÁLIDOS
disparoValido1 = Disparo{posicaoDisparo=(1,4), direcaoDisparo=Norte, tipoDisparo=Mina, tempoDisparo=Just 2, donoDisparo=0}
disparoValido2 = Disparo{posicaoDisparo=(3,2), direcaoDisparo=Sul, tipoDisparo=Dinamite, tempoDisparo=Just 4, donoDisparo=1}
disparoValido3 = Disparo{posicaoDisparo=(5,5), direcaoDisparo=Este, tipoDisparo=Bazuca, tempoDisparo=Nothing, donoDisparo=2}
disparoValido4 = Disparo{posicaoDisparo=(2,3), direcaoDisparo=Norte, tipoDisparo=Bazuca, tempoDisparo=Nothing, donoDisparo=1} -- perfura terreno opaco se posição anterior não for opaca
disparoValido5 = Disparo{posicaoDisparo=(0,0), direcaoDisparo=Sul, tipoDisparo=Mina, tempoDisparo=Just 0, donoDisparo=3}
disparoValido6 = Disparo{posicaoDisparo=(4,1), direcaoDisparo=Oeste, tipoDisparo=Dinamite, tempoDisparo=Just 2, donoDisparo=0}

-- DISPAROS INVÁLIDOS
disparoInvalido1 = Disparo{posicaoDisparo=(-1,3), direcaoDisparo=Norte, tipoDisparo=Mina, tempoDisparo=Just 1, donoDisparo=0} -- fora do mapa
disparoInvalido2 = Disparo{posicaoDisparo=(2,2), direcaoDisparo=Este, tipoDisparo=Mina, tempoDisparo=Just 0, donoDisparo=1} -- terreno opaco
disparoInvalido3 = Disparo{posicaoDisparo=(3,3), direcaoDisparo=Norte, tipoDisparo=Jetpack, tempoDisparo=Nothing, donoDisparo=1} -- arma não gera disparo
disparoInvalido4 = Disparo{posicaoDisparo=(4,4), direcaoDisparo=Sul, tipoDisparo=Dinamite, tempoDisparo=Just 5, donoDisparo=2} -- tempo inválido
disparoInvalido5 = Disparo{posicaoDisparo=(5,5), direcaoDisparo=Norte, tipoDisparo=Mina, tempoDisparo=Just 1, donoDisparo=0} -- dono já tem mina ativa
disparoInvalido6 = Disparo{posicaoDisparo=(6,6), direcaoDisparo=Sul, tipoDisparo=Bazuca, tempoDisparo=Nothing, donoDisparo=1} -- posição anterior também opaca
disparoInvalido7 = Disparo{posicaoDisparo=(1,1), direcaoDisparo=Norte, tipoDisparo=Dinamite, tempoDisparo=Nothing, donoDisparo=2} -- tempo obrigatório para dinamite
disparoInvalido8 = Disparo{posicaoDisparo=(2,4), direcaoDisparo=Oeste, tipoDisparo=Mina, tempoDisparo=Just 1, donoDisparo=10} -- dono inválido (fora do índice)
disparoInvalido9 = Disparo{posicaoDisparo=(3,5), direcaoDisparo=Sul, tipoDisparo=Mina, tempoDisparo=Just 1, donoDisparo=0} -- mina sobre barril

-- BARRIS VÁLIDOS
barrilValido1 = Barril{posicaoBarril=(4,6),explodeBarril = False }
barrilValido2 = Barril{posicaoBarril=(0,5),explodeBarril = False }
barrilValido3 = Barril{posicaoBarril=(2,0),explodeBarril = False }

-- BARRIS INVÁLIDOS
barrilInvalido1 = Barril{posicaoBarril=(4,5),explodeBarril = False } -- mesma posição que outro barril
barrilInvalido2 = Barril{posicaoBarril=(3,2),explodeBarril = False } -- posição ocupada por minhoca
barrilInvalido3 = Barril{posicaoBarril=(2,2),explodeBarril = False } -- posição em terreno opaco (opcional para teste)

-- CASOS ESPECIAIS
disparoValido7 = Disparo{posicaoDisparo=(7,7), direcaoDisparo=Norte, tipoDisparo=Mina, tempoDisparo=Just 1, donoDisparo=1} -- múltiplos tipos diferentes do mesmo dono permitido
disparoInvalido10 = Disparo{posicaoDisparo=(7,7), direcaoDisparo=Norte, tipoDisparo=Mina, tempoDisparo=Just 1, donoDisparo=1} -- repetição do mesmo tipo já existente para o mesmo dono


-- * -----------------------------------------
-- * MINHOCAS
-- * -----------------------------------------

-- MINHOCAS VÁLIDAS
minhocaValida1 = Minhoca{posicaoMinhoca=Just (3,0), vidaMinhoca=Morta, jetpackMinhoca=100, escavadoraMinhoca=200, bazucaMinhoca=150, minaMinhoca=3, dinamiteMinhoca=1} -- posição válida, morta, munições >= 0
minhocaValida2 = Minhoca{posicaoMinhoca=Just (1,1), vidaMinhoca=Viva 50, jetpackMinhoca=0, escavadoraMinhoca=0, bazucaMinhoca=0, minaMinhoca=0, dinamiteMinhoca=0} -- viva, vida entre 0-100, posição livre
minhocaValida3 = Minhoca{posicaoMinhoca=Nothing, vidaMinhoca=Morta, jetpackMinhoca=10, escavadoraMinhoca=5, bazucaMinhoca=2, minaMinhoca=0, dinamiteMinhoca=0} -- sem posição, obrigatoriamente morta
minhocaValida4 = Minhoca{posicaoMinhoca=Just (2,2), vidaMinhoca=Viva 0, jetpackMinhoca=0, escavadoraMinhoca=0, bazucaMinhoca=0, minaMinhoca=0, dinamiteMinhoca=0} -- viva com vida 0 (permitido), posição válida
minhocaValida5 = Minhoca{posicaoMinhoca=Just (0,0), vidaMinhoca=Viva 100, jetpackMinhoca=50, escavadoraMinhoca=50, bazucaMinhoca=50, minaMinhoca=5, dinamiteMinhoca=2} -- posição válida, vida máxima

-- MINHOCAS INVÁLIDAS
minhocaInvalida1 = Minhoca{posicaoMinhoca=Just (3,0), vidaMinhoca=Viva 101, jetpackMinhoca=10, escavadoraMinhoca=5, bazucaMinhoca=2, minaMinhoca=0, dinamiteMinhoca=0} -- vida > 100
minhocaInvalida2 = Minhoca{posicaoMinhoca=Just (1,1), vidaMinhoca=Viva (-1), jetpackMinhoca=10, escavadoraMinhoca=5, bazucaMinhoca=2, minaMinhoca=0, dinamiteMinhoca=0} -- vida < 0
minhocaInvalida3 = Minhoca{posicaoMinhoca=Just (6,6), vidaMinhoca=Viva 50, jetpackMinhoca=10, escavadoraMinhoca=5, bazucaMinhoca=2, minaMinhoca=0, dinamiteMinhoca=0} -- posição ocupada por outro barril
minhocaInvalida4 = Minhoca{posicaoMinhoca=Just (0,0), vidaMinhoca=Viva 50, jetpackMinhoca=(-5), escavadoraMinhoca=5, bazucaMinhoca=2, minaMinhoca=0, dinamiteMinhoca=0} -- munição negativa
minhocaInvalida5 = Minhoca{posicaoMinhoca=Nothing, vidaMinhoca=Viva 50, jetpackMinhoca=10, escavadoraMinhoca=5, bazucaMinhoca=2, minaMinhoca=0, dinamiteMinhoca=0} -- sem posição mas viva (inválido)
minhocaInvalida6 = Minhoca{posicaoMinhoca=Just (2,2), vidaMinhoca=Viva 50, jetpackMinhoca=10, escavadoraMinhoca=5, bazucaMinhoca=2, minaMinhoca=0, dinamiteMinhoca=0} -- posição ocupada por outra minhoca
minhocaInvalida7 = Minhoca{posicaoMinhoca=Just (1,3), vidaMinhoca=Viva 50, jetpackMinhoca=10, escavadoraMinhoca=5, bazucaMinhoca=2, minaMinhoca=0, dinamiteMinhoca=0} -- posição em água mas viva (inválido)


-- * -----------------------------------------
-- * ESTADOS
-- * -----------------------------------------

estado1 = Estado{mapaEstado=mapaOk, objetosEstado=[disparoValido1, disparoValido2, barrilValido1], minhocasEstado=[minhocaValida1, minhocaValida2]} -- estado totalmente válido

estado2 = Estado{mapaEstado=mapaVazio, objetosEstado=[disparoValido1], minhocasEstado=[minhocaValida1]} -- mapa vazio (inválido)

estado3 = Estado{mapaEstado=mapaInvalido1, objetosEstado=[disparoValido1], minhocasEstado=[minhocaValida1]} -- mapa com tamanho incorreto (inválido)

estado4 = Estado{mapaEstado=mapaInvalido2, objetosEstado=[disparoValido1], minhocasEstado=[minhocaValida1]} -- mapa com terreno inválido (Areia)

estado5 = Estado{mapaEstado=mapaOk, objetosEstado=[disparoInvalido1], minhocasEstado=[minhocaValida1]} -- disparo fora do mapa

estado6 = Estado{mapaEstado=mapaOk, objetosEstado=[disparoInvalido2], minhocasEstado=[minhocaValida1]} -- disparo sobre terreno opaco

estado7 = Estado{mapaEstado=mapaOk, objetosEstado=[disparoInvalido3], minhocasEstado=[minhocaValida1]} -- disparo de arma instantânea (Jetpack)

estado8 = Estado{mapaEstado=mapaOk, objetosEstado=[disparoInvalido4], minhocasEstado=[minhocaValida1]} -- tempo inválido

estado9 = Estado{mapaEstado=mapaOk, objetosEstado=[disparoInvalido5], minhocasEstado=[minhocaValida1]} -- dono já tem disparo do mesmo tipo

estado10 = Estado{mapaEstado=mapaOk, objetosEstado=[disparoInvalido6], minhocasEstado=[minhocaValida1]} -- bazuca perfura terreno inválido

estado11 = Estado{mapaEstado=mapaOk, objetosEstado=[disparoInvalido7], minhocasEstado=[minhocaValida1]} -- tempo obrigatório faltando

estado12 = Estado{mapaEstado=mapaOk, objetosEstado=[disparoInvalido8], minhocasEstado=[minhocaValida1]} -- dono inválido

estado13 = Estado{mapaEstado=mapaOk, objetosEstado=[disparoInvalido9], minhocasEstado=[minhocaValida1]} -- disparo sobre barril

estado14 = Estado{mapaEstado=mapaOk, objetosEstado=[barrilInvalido1], minhocasEstado=[minhocaValida1]} -- barril em mesma posição que outro

estado15 = Estado{mapaEstado=mapaOk, objetosEstado=[barrilInvalido2], minhocasEstado=[minhocaValida1]} -- barril sobre minhoca

estado16 = Estado{mapaEstado=mapaOk, objetosEstado=[barrilInvalido3], minhocasEstado=[minhocaValida1]} -- barril sobre terreno opaco

estado17 = Estado{mapaEstado=mapaOk, objetosEstado=[disparoValido7], minhocasEstado=[minhocaValida1]} -- múltiplos disparos diferentes do mesmo dono

estado18 = Estado{mapaEstado=mapaOk, objetosEstado=[disparoInvalido10], minhocasEstado=[minhocaValida1]} -- repetição do mesmo tipo de disparo para o mesmo dono

estado19 = Estado{mapaEstado=mapaOk, objetosEstado=[disparoValido1, barrilValido1], minhocasEstado=[minhocaInvalida1]} -- minhoca com vida > 100

estado20 = Estado{mapaEstado=mapaOk, objetosEstado=[disparoValido1, barrilValido1], minhocasEstado=[minhocaInvalida2]} -- minhoca com vida < 0

estado21 = Estado{mapaEstado=mapaOk, objetosEstado=[disparoValido1, barrilValido1], minhocasEstado=[minhocaInvalida3]} -- minhoca sobre barril

estado22 = Estado{mapaEstado=mapaOk, objetosEstado=[disparoValido1, barrilValido1], minhocasEstado=[minhocaInvalida4]} -- munição negativa

estado23 = Estado{mapaEstado=mapaOk, objetosEstado=[disparoValido1, barrilValido1], minhocasEstado=[minhocaInvalida5]} -- minhoca sem posição mas viva

estado24 = Estado{mapaEstado=mapaOk, objetosEstado=[disparoValido1, barrilValido1], minhocasEstado=[minhocaInvalida6]} -- posição ocupada por outra minhoca

estado25 = Estado{mapaEstado=mapaOk, objetosEstado=[disparoValido1, barrilValido1], minhocasEstado=[minhocaInvalida7]} -- posição em água mas viva

estado26 = Estado{mapaEstado=mapaOk, objetosEstado=[disparoValido2, disparoValido3], minhocasEstado=[minhocaValida3, minhocaValida4]} -- minhoca sem posição + viva com vida 0

estado27 = Estado{mapaEstado=mapaOk, objetosEstado=[disparoInvalido1, barrilValido2], minhocasEstado=[minhocaValida1]} -- disparo fora do mapa + barril válido

estado28 = Estado{mapaEstado=mapaOk, objetosEstado=[disparoValido4, barrilInvalido2], minhocasEstado=[minhocaValida2]} -- disparo perfura terreno + barril sobre minhoca

estado29 = Estado{mapaEstado=mapaOk, objetosEstado=[disparoInvalido3, disparoValido5], minhocasEstado=[minhocaInvalida5]} -- disparo de arma instantânea + minhoca sem posição mas viva

estado30 = Estado{mapaEstado=mapaOk, objetosEstado=[disparoValido6], minhocasEstado=[minhocaInvalida6]} -- disparo válido + minhoca em posição ocupada

estado31 = Estado{mapaEstado=mapaOk, objetosEstado=[disparoInvalido4], minhocasEstado=[minhocaValida5]} -- tempo inválido + minhoca viva máxima

estado32 = Estado{mapaEstado=mapaOk, objetosEstado=[disparoInvalido7, barrilValido3], minhocasEstado=[minhocaValida1]} -- tempo obrigatório faltando + barril válido

estado33 = Estado{mapaEstado=mapaOk, objetosEstado=[disparoValido7, barrilValido1], minhocasEstado=[minhocaValida2]} -- múltiplos disparos diferentes + barril válido

estado34 = Estado{mapaEstado=mapaOk, objetosEstado=[disparoInvalido10], minhocasEstado=[minhocaInvalida2]} -- repetição do mesmo tipo + minhoca vida <0

estado35 = Estado{mapaEstado=mapaOk, objetosEstado=[disparoValido1, barrilInvalido3], minhocasEstado=[minhocaValida3]} -- disparo válido + barril sobre terreno opaco + minhoca sem posição morta

estado36 = Estado{mapaEstado=mapaOk, objetosEstado=[disparoValido5, disparoValido6], minhocasEstado=[minhocaInvalida7]} -- disparos válidos + minhoca em água mas viva

estado37 = Estado{mapaEstado=mapaOk, objetosEstado=[disparoInvalido2, disparoInvalido9], minhocasEstado=[minhocaValida4]} -- disparo sobre terreno opaco + disparo sobre barril + minhoca viva vida 0

estado38 = Estado{mapaEstado=mapaOk, objetosEstado=[disparoValido3, disparoValido4, barrilValido2], minhocasEstado=[minhocaValida5, minhocaValida1]} -- combinação múltipla de disparos válidos e barril, minhocas válidas

estado39 = Estado{mapaEstado=mapaOk, objetosEstado=[disparoInvalido8], minhocasEstado=[minhocaInvalida3]} -- dono inválido + minhoca sobre barril

estado40 = Estado{mapaEstado=mapaOk, objetosEstado=[disparoInvalido6, disparoValido1], minhocasEstado=[minhocaValida2]} -- bazuca perfura terreno errado + disparo válido

-- * -----------------------------------------
-- * LISTA DE TODOS OS ESTADOS
-- * -----------------------------------------

todosEstados = [
    estado1,
    estado2,
    estado3,
    estado4,
    estado5,
    estado6,
    estado7,
    estado8,
    estado9,
    estado10,
    estado11,
    estado12,
    estado13,
    estado14,
    estado15,
    estado16,
    estado17,
    estado18,
    estado19,
    estado20,
    estado21,
    estado22,
    estado23,
    estado24,
    estado25,
    estado26,
    estado27,
    estado28,
    estado29,
    estado30,
    estado31,
    estado32,
    estado33,
    estado34,
    estado35,
    estado36,
    estado37,
    estado38,
    estado39,
    estado40
    ]