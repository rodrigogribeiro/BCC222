> import Data.Char

List Comprehensions
===================

- Notação para listas inspirada
na teoria de conjuntos

- Exemplo: somar o quadrado de
números ímpares no intervalo [1,20]:

> ex1 :: Int
> ex1 = sum [x ^ 2 | x <- [1..20], odd x]

- Componentes de um list comprehension:
    [ x ^ 2 | .... ]          : expressão do resultado

    [...  | x <- [1..20] ...] : gerador

    [...  | ... , odd x     ] : condição de seleção

Strings como listas
===================

> toLowers :: String -> String
> toLowers xs = [toLower c | c <- xs]

> selectDigits :: String -> String
> selectDigits xs = [c | c <- xs, isDigit c]


Produto cartesiano
==================

> (.*.) :: [a] -> [b] -> [(a,b)] -- operadores binários
> xs .*. ys = [(x,y) | x <- xs, y <- ys]

Triplas Pitagóricas
===================

(x,y,z) é pitagórica se x ^ 2 == y ^ 2 + z ^ 2

> triples :: Int -> [(Int, Int, Int)]
> triples n
>    = [(x,y,z) | x <- [1..n]
>               , y <- [1..n]
>               , z <- [1..n]
>               , (x ^ 2) == (y ^ 2) + (z ^ 2)]


Gerando números primos
====================

- Fatorando um número inteiro

> factors :: Int -> [Int]
> factors n = [x | x <- [1..n] , n `mod` x == 0]

- Determinando se um número é primo

> prime :: Int -> Bool
> prime n = factors n == [1,n]

- Números primos menores ou iguais a n

> primes :: Int -> [Int]
> primes n = [x | x <- [2..n] , prime x]

