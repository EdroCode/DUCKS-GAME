
-- dedicada ao teste de funçcões para melhor entendimento da linguagem
type Posicao = (Int,Int)


encontraLista :: Int -> [a] -> Bool
encontraLista i [] = False
encontraLista i (h:t) = if i == 0 then True else encontraLista (i-1) t

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


