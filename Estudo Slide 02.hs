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

{-Escreva uma função para retornar, em forma de tabela, todas as
vendas da semana 0 até a semana n, incluindo o total e a média de
vendas no período. Usem as funções definidas previamente e
defina novas funções que achar necessário.
Semana Venda
  0     12
  1     14
  2     15
Total   41
Média   13.6667-}

totalVendas :: Int -> Int
totalVendas a
  | a == 0 = vendas 0
  | otherwise = totalVendas (a - 1) + vendas a


imprimeTabela :: Int->String
imprimeTabela n = cabecalho 
                ++ imprimeSemanas n 
                ++ imprimeTotal n 
                ++ imprimeMedia n


cabecalho = "Semana Venda"

imprimeSemanas :: Int -> String
imprimeSemanas 0 = "  "++"0"++"     "++ show(vendas 0)
imprimeSemanas n = "  "++show(n)++"     "++show(vendas n) ++ imprimeSemanas (n-1)

imprimeTotal :: Int -> String
imprimeTotal n = "Total   "
                ++ show (totalVendas n)

imprimeMedia :: Int -> String

imprimeMedia n = "Media   " ++ show (media n)


media a = totalVendas (a) / a



