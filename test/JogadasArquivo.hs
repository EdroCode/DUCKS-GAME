module JogadasArquivo where
import Labs2025

mapaValido =
  [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar],
   [Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar],
   [Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar],
   [Terra,Terra,Terra,Terra,Terra,Terra,Terra,Agua,Agua,Agua],
   [Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Agua,Agua]]

estado1 = Estado mapaValido [] [Minhoca (Just (2,2)) (Viva 100) 1 1 1 1 1]
estado2 = Estado mapaValido [] [Minhoca (Just (2,2)) (Viva 100) 1 1 1 1 1, Minhoca (Just (2,4)) (Viva 100) 1 1 1 1 1]
estado3 = Estado mapaValido [] [Minhoca (Just (0,0)) (Viva 100) 1 1 1 1 1, Minhoca (Just (0,3)) (Viva 100) 1 1 1 1 1]
estado4 = Estado mapaValido [] [Minhoca (Just (2,4)) (Viva 100) 1 1 1 1 1]
estado5 = Estado mapaValido [] [Minhoca (Just (2,2)) (Viva 100) 1 1 1 1 1]
estado6 = Estado mapaValido [] [Minhoca (Just (2,4)) (Viva 100) 1 1 1 1 1, Minhoca (Just (2,3)) (Viva 100) 1 1 1 1 1]
estado7 = Estado mapaValido [] [Minhoca (Just (2,2)) Morta 1 1 1 1 1]
estado8 = Estado mapaValido [] [Minhoca (Just (2,4)) (Viva 100) 1 1 1 1 1]
estado9 = Estado mapaValido [] [Minhoca (Just (1,1)) Morta 1 1 1 1 1]
estado10 = Estado mapaValido [] [Minhoca (Just (2,7)) (Viva 100) 1 1 1 1 1, Minhoca (Just (2,8)) (Viva 100) 1 1 1 1 1]
estado11 = Estado mapaValido [] [Minhoca (Just (2,3)) (Viva 100) 1 1 1 1 1]
estado12 = Estado mapaValido [] [Minhoca (Just (0,0)) (Viva 100) 1 1 1 1 1]
estado13 = Estado mapaValido [] [Minhoca (Just (2,2)) (Viva 100) 1 1 1 1 1]
estado14 = Estado mapaValido [] [Minhoca (Just (2,2)) (Viva 100) 1 1 1 1 1, Minhoca (Just (2,3)) (Viva 100) 1 1 1 1 1]
estado15 = Estado mapaValido [] [Minhoca (Just (2,2)) (Viva 100) 1 1 1 1 1]
estado16 = Estado mapaValido [] [Minhoca (Just (2,4)) (Viva 100) 1 1 1 1 1]
estado17 = Estado mapaValido [] [Minhoca (Just (2,2)) (Viva 100) 1 1 1 1 1]
estado18 = Estado mapaValido [] [Minhoca (Just (2,2)) (Viva 100) 1 1 1 1 1, Minhoca (Just (2,3)) (Viva 100) 1 1 1 1 1]
estado19 = Estado mapaValido [] [Minhoca (Just (2,6)) (Viva 100) 1 1 1 1 1, Minhoca (Just (2,3)) (Viva 100) 1 1 1 1 1]
estado20 = Estado mapaValido [] [Minhoca (Just (2,4)) (Viva 100) 1 1 1 1 1]
estado21 = Estado mapaValido [] [Minhoca (Just (0,9)) (Viva 100) 1 1 1 1 1, Minhoca (Just (0,8)) (Viva 100) 1 1 1 1 1]
estado22 = Estado mapaValido [] [Minhoca (Just (2,3)) (Viva 100) 1 1 1 1 1]
estado23 = Estado mapaValido [] [Minhoca (Just (2,2)) (Viva 100) 1 1 1 1 1]
estado24 = Estado mapaValido [] [Minhoca (Just (2,3)) (Viva 100) 1 1 1 1 1]
estado25 = Estado mapaValido [Disparo (3,3) Sul Bazuca Nothing 0] [Minhoca (Just (3,3)) (Viva 100) 1 1 1 1 1, Minhoca (Just (3,4)) (Viva 100) 1 1 1 1 1]
estado26 = Estado mapaValido [] [Minhoca (Just (2,2)) (Viva 100) 1 1 1 1 1, Minhoca (Just (2,3)) (Viva 100) 1 1 1 1 1]
estado27 = Estado mapaValido [] [Minhoca (Just (2,2)) (Viva 100) 0 0 0 0 0]
estado28 = Estado mapaValido [] [Minhoca (Just (2,2)) (Viva 100) 1 1 1 1 1]
estado29 = Estado mapaValido [] [Minhoca (Just (2,2)) (Viva 100) 1 1 1 1 1]
estado30 = Estado mapaValido [] [Minhoca (Just (2,2)) (Viva 100) 1 1 1 1 1]
estado31 = Estado mapaValido [] [Minhoca (Just (23,2)) (Viva 100) 1 1 1 1 1]



jogadasTeste =
  [ (0 :: Int, Move Norte, estado1)
  , (1 :: Int, Move Sul, estado2)
  , (0 :: Int, Move Este, estado3)
  , (0 :: Int, Move Oeste, estado4)
  , (0 :: Int, Move Nordeste, estado5)
  , (1 :: Int, Move Noroeste, estado6)
  , (0 :: Int, Move Sudeste, estado7)
  , (0 :: Int, Move Sudoeste, estado8)
  , (0 :: Int, Move Norte, estado9)
  , (1 :: Int, Move Sul, estado10)
  , (0 :: Int, Move Este, estado11)
  , (0 :: Int, Move Oeste, estado12)
  , (0 :: Int, Dispara Jetpack Norte, estado13)
  , (1 :: Int, Dispara Jetpack Sul, estado14)
  , (0 :: Int, Dispara Jetpack Este, estado15)
  , (0 :: Int, Dispara Jetpack Oeste, estado16)
  , (0 :: Int, Dispara Escavadora Norte, estado17)
  , (1 :: Int, Dispara Escavadora Sul, estado18)
  , (0 :: Int, Dispara Bazuca Este, estado19)
  , (0 :: Int, Dispara Bazuca Oeste, estado20)
  , (0 :: Int, Dispara Mina Norte, estado21)
  , (0 :: Int, Dispara Mina Sul, estado22)
  , (0 :: Int, Dispara Dinamite Este, estado23)
  , (0 :: Int, Dispara Dinamite Oeste, estado24)
  , (0 :: Int, Dispara Bazuca Sul, estado25)
  , (0 :: Int, Dispara Jetpack Norte, estado26)
  , (0 :: Int, Dispara Jetpack Norte, estado27)
  , (0 :: Int, Dispara Jetpack Norte, estado28)
  , (0 :: Int, Dispara Escavadora Norte, estado29)
  , (0 :: Int, Dispara Dinamite Norte, estado30)
  , (0 :: Int, Move Norte, estado27)
  , (0 :: Int, Move Nordeste, estado28)
  , (0 :: Int, Move Norte, estado10)
  , (0 :: Int, Move Norte, estado21)
  , (0 :: Int, Move Oeste, estado6)
  , (0 :: Int, Move Norte, estado7)
  , (0 :: Int, Move Norte, estado9)
  , (0 :: Int, Move Norte, estado31)
  , (0 :: Int, Dispara Jetpack Norte, estado27)
  , (0 :: Int, Dispara Bazuca Sul, estado25)
  , (0 :: Int, Dispara Escavadora Norte, estado26)
  , (0 :: Int, Dispara Mina Norte, estado6)
  , (0 :: Int, Dispara Dinamite Norte, estado6)
  , (0 :: Int, Dispara Bazuca Norte, estado21)
  , (0 :: Int, Dispara Jetpack Oeste, estado6)
  , (0 :: Int, Dispara Jetpack Sul, estado10)
  , (0 :: Int, Dispara Jetpack Norte, estado3)
  , (0 :: Int, Dispara Escavadora Sul, estado5)
  , (0 :: Int, Dispara Mina Este, estado5)
  , (0 :: Int, Dispara Dinamite Oeste, estado31)
  ]
