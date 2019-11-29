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

--Sistema de controle de vendas

vendas 0 = 2
vendas 1 = 1
vendas 2 = 10
vendas 3 = 8
vendas 4 = 5
vendas 5 = 7


totalVendas :: Int -> Int
totalVendas a
  | a == 0 = vendas 0
  | otherwise = totalVendas (a - 1) + vendas a

maxVendas :: Int -> Int

maxVendas a
  | a ==0 = vendas 0
  | otherwise = maxi (maxVendas (a - 1)) (vendas a)

{- 
maxVendas 2
maxi(maxVendas 1) vendas 2
maxi (maxi (maxVendas(0)) (vendas 1)) 10
maxi (maxi 2 1) 10
maxi 2 10
10
-}

{- Outras formas de definir recursão
maxVendas :: Int -> Int
maxVendas 0 = vendas 0
maxVendas a = maxi ( maxVendas (a-1)) (vendas a)
-}

-- Casamento de padrão

myNot :: Bool -> Bool
myNot True = False
myNot False = True

myAnd :: Bool -> Bool -> Bool
myAnd False x = False
myAnd True x = x

myOr :: Bool -> Bool -> Bool
myOr False False = False
myOr x y = True

-- f n + 1 == (f n) + 1

--Exercícios

fat :: Int -> Int
fat 1 = 1
fat a = a * (fat (a-1))

all4Equal :: Int -> Int -> Int -> Int -> Bool
all4Equal a b c d
  | (a==b)&&(c==d)&&(b==c) = True
  | otherwise = False

equalCount :: Int -> Int-> Int -> Int
equalCount a b c
  | (a==b)&&(b==c) = 3
  | (a==b)||(a==c)||(b==c) = 2
  | otherwise = 0

