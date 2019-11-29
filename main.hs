--Introdução
--Programação baseadas em definições

answer :: Int 
answer = 42

greater :: Bool
greater = (answer >71)

--Definição de funções

square :: Int -> Int
square x = x * x

allEqual :: Int -> Int -> Int -> Bool
allEqual a b c = (a == b) && (b == c)

maxi :: Int -> Int -> Int
maxi a b
  | a >= b = a
  | otherwise = b

-- Aplicando funções

--square 5 -- 25
--square (5) -- 25

--allEqual 1 2 3 -- False
--allEqual (1,2,3) -- Erro! Pois (1,2,3) é uma tripla, e não 3 valores

