--Tipos Básicos em Haskell

-- Int, Bool, Funções...

import Data.Char

--Ou Exclusivo
exOr :: Bool -> Bool -> Bool
exOr a b = (a || b) && not (a && b)

vendas 0 = 2
vendas 1 = 1
vendas 2 = 10
vendas 3 = 8
vendas 4 = 5
vendas 5 = 7
vendas 6 = 0

--verificando se não houve vendas na semana n

vendasNulas :: Int -> Bool
vendasNulas a = (vendas a == 0)

--Caracteres
-- import Data.Char == importando biblioteca
--fromEnum :: Char -> Int  == descobre o código do char
--toEnum :: Int-> Char == transforma um código no char correspondente

offset = fromEnum 'A' - fromEnum 'a'

--tranformar em maiuscula

maiuscula :: Char -> Char
maiuscula a = toEnum (fromEnum a + offset)

ehDigito :: Char -> Bool
ehDigito a = (a <= '9') && (a >= '0')

--Strings
--"pedro", "olá"
-- show (9) == "9"          ===> transforma em string
-- (read "3") :: Int == 3   ===> transfora em Int

--Float e Double
--22.34565


--Exercícios

{-Defina a funçãoo addEspacos que produz um string com uma
quantidade n de espaços.-}

addEspacos :: Int -> String
addEspacos 0 = ""
addEspacos a = " " ++ addEspacos (a-1)

{-Defina a função paraDireita utilizando a definição de
addEspacos para adiciconar uma quantidade n de espaços à
esquerda de um dado String, movendo o mesmo para a direita.-}

paraDireita :: Int -> String -> String
paraDireita num texto = (addEspacos num) ++ texto


