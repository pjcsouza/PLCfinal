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

imprimeMedia n = "Media   " ++ show (n) --deveria ser media mas ta dando erro


-- media a = totalVendas (a) / a


--Tuplas

intP :: (Int, Int) -- != Int->Int
intP = (33,43)

addPair :: (Int,Int) -> Int
addPair (x,y) = x+y

--Sinonimos de Tipos

type Name = String
type Age = Int
type Phone = Int
type Person = (Name, Age, Phone)

novaPessoa :: String -> Int -> Int-> Person
novaPessoa a b c = (a,b,c)


name :: Person -> String
name (a,b,c) = a


--Exercícios
{-
Defina a função menorMaior que recebe três inteiros e retorna
uma tupla com o menor e o maior deles, respectivamente.
-}

menorMaior :: Int->Int->Int->(Int,Int)
menorMaior a b c = ((menor a b c), (maior a b c))

menor ::Int->Int->Int->Int
menor a b c
  | (a<=b)&&(a<=c) = a
  | (b<=a)&&(b<=c) = b
  | otherwise = c

maior ::Int->Int->Int->Int
maior a b c
  | (a>=b)&&(a>=c) = a
  | (b>=a)&&(b>=c) = b
  | otherwise = c

{-
Uma linha pode ser representada da seguinte forma
type Ponto = ( Floa t , Fl o a t )
type Reta = ( Ponto , Ponto )
• Defina funções que
• retornem
• a primeira coordenada de um ponto
• a segunda coordenada de um ponto
• indique se uma reta é vertical ou não ( x1 = x2)
-}

type Ponto = ( Float , Float )
type Reta = ( Ponto , Ponto )

primeiraCoordenada :: Ponto ->Float
primeiraCoordenada (x,y) = x

segundaCoordenada :: Ponto ->Float
segundaCoordenada (x,y) = y

vertical :: Reta -> Bool
vertical (a,b) = primeiraCoordenada a == primeiraCoordenada a


