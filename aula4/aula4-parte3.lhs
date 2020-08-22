> import Prelude hiding (map)

Tarefas
=======

- Dobrar todos os números em uma lista

- Negar todos os booleanos em uma lista

Receita
-------

1. Definir o tipo
2. Enumerar os casos
3. Definir casos base.
4. Definir casos recursivos.

> doubleList :: [Int] -> [Int]
> doubleList []       = [] -- ***
> doubleList (x : xs) = 2 * x : doubleList xs

-- 2 * ...

> notList :: [Bool] -> [Bool]
> notList []       = []    -- ***
> notList (x : xs) = not x : notList xs

-- not ...

> map :: (a -> b) -> [a] -> [b]
> map _ []       = []
> map f (x : xs) = f x : map f xs

> doubleList' xs
>    = map double xs
>      where
>        double x = 2 * x

> notList' xs = map not xs
