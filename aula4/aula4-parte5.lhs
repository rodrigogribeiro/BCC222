> import Prelude hiding (filter)
> import Data.Char (isLower)

Tarefas
=======

- Implementar uma função para retornar
todos os inteiros pares em uma lista.

- Implementar uma função para retornar
todas as letras minúsculas em uma string.

- Não é permitido usar list comprehensions.

Função evens
============

> evens :: [Int] -> [Int]
> evens []       = []
> evens (x : xs)
>    | even x    = x : evens xs
>    | otherwise = evens xs


Função lowers
=============

> lowers :: String -> String
> lowers []       = []
> lowers (x : xs)
>   | isLower x = x : lowers xs
>   | otherwise = lowers xs

even    :: Int  -> Bool
isLower :: Char -> Bool
........................
p       :: a    -> Bool

Generalizando
=============

> filter :: (a -> Bool) -> [a] -> [a]
> filter _ []       = []
> filter p (x : xs)
>   | p x = x : filter p xs
>   | otherwise = filter p xs


> evens' = filter even

> lowers' = filter isLower
