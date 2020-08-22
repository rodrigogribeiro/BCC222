> import Prelude
>     hiding (sum, and, concat
>            , foldr, filter
>            , takeWhile, all
>            , map, length )

Tarefas
=======

- Somar os elementos de uma lista de números.

- Conjunção de uma lista de booleanos.

- Concatenação de uma lista de listas.


> sum :: [Int] -> Int
> sum = foldr (+) 0

> and :: [Bool] -> Bool
> and = foldr (&&) True

> concat :: [[a]] -> [a]
> concat = foldr (++) []

> foldr :: (a -> b -> b) -> b -> [a] -> b
> foldr _ v []       = v
> foldr f v (x : xs) = f x (foldr f v xs)

foldr (+) 0 [1,2,3] ==
foldr (+) 0 (1 : (2 : (3 : []))) ==
       f  v  x   xs
1 + (foldr (+) 0 (2 : (3 : []))) ==
                  x   xs
1 + (2 + (foldr (+) 0 (3 : []))) ==
                       x   xs
1 + (2 + (3 + (foldr (+) 0 []))) ==

1 :  2 :  3 : []
1 + (2 + (3 + 0))

Definindo outras funções usando foldr
=====================================

Template
---------

foo = foldr step base
      where
        step x ac =...

> map :: (a -> b) -> [a] -> [b]
> map f = foldr step []
>   where
>     step x ac = f x : ac

> length :: [a] -> Int
> length = foldr (\ _ ac -> 1 + ac) 0

> filter :: (a -> Bool) -> [a] -> [a]
> filter p = foldr step []
>   where
>     step x ac = if p x
>                 then x : ac
>                 else ac

> takeWhile :: (a -> Bool) -> [a] -> [a]
> takeWhile p = foldr step []
>        where
>          step x ac = if p x then x : ac
>                      else []
