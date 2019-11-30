--Slide 03
--Listas

--Coleções devem ser do mesmo tipo
--[1,2,3,4,5] :: [Int]
--"pedrojose" :: [Char] -- String é uma lista de Char


--Construtor de Lista

-- [5] == 5:[]
-- [4,5] == 4:(5:[])

--[2..7] == [2,3,4,5,6,7]
--['a'..'d'] == ['a','b','c','d']
-- [7,5..0] == [7,5,3,1] --Determina-se o Step na diferená entre 7 e 5

-- Exercícios
{-
Quantos itens existem nas seguintes listas?
• [2,3]
R: 2 inteiros.

• [[2,3]]
R: 1 lista.

Qual o tipo de [[2,3]] ?
R: Lista de lista de inteiro.

• Qual o resultado da avaliação de

• [2,4..9]
R: [2,4,6,8]   Obs.: Step de (4-2) = 2

• [2..2]
R: [2]

• [2,7..4]
R:[2]

• [10,9..1]
R:[10,9,8,7,6,5,4,3,2,1]   Obs.: Step de -1

• [10..1]
R: [], pois não há step, com isso não monta-se automaticamente listas decrescentes.

• [2,9,8..1]
R: ERRO! Existe 3 elementos para o step, o que é inválido.
-}


--Funções sobre listas

sumList :: [Int]->Int
sumList [] = 0
sumList (x:xs) = x + sumList xs 

-- x é a cabeça da lista e xs é a cauda.
-- Ex.: [2,3,4,5,6]  ==> 2 é a cabeça e [3,4,5,6] é a cauda.

--Avaliando sumList [6,5,2]
{-
sumList [6,5,2]
6 + sumList [5,2]
6 + 5 + sumList [2]
6 + 5 + 2 + sumList []
6 + 5 + 2 + 0 = 13
-}

--Exercícios
--dobrar os elementos de uma lista: double :: [Int] −> [Int]

double :: [Int]->[Int]
double [] = []
double (x:xs) = (x * 2) : double xs

--determinar se um valor faz parte de uma lista: member :: [Int] −> Int −> Bool

member :: [Int]->Int -> Bool
member [] m = False
member (x:xs) m = (x == m) || member xs m

--filtrar apenas os números de uma lista: digits :: String −> String

ehDigito :: Char -> Bool
ehDigito a = (a <= '9') && (a >= '0')

digits :: String -> String
digits "" = ""
digits (x:xs)
    | ehDigito x = x : digits xs
    | otherwise = digits xs


--somar uma lista de pares: sumPairs :: [( Int, Int)]−>[Int]

sumPairs :: [(Int, Int)] -> [Int]
sumPairs [] = []
sumPairs (x:xs) = (fst(x)+snd(x)) : sumPairs xs


--Casamento de Padrão

maiorLista :: [Int] -> Int
maiorLista [] = minBound :: Int
maiorLista [x] = x
maiorLista (x:xs)
    | x > maiorLista xs = x
    | otherwise = maiorLista xs