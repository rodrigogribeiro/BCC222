> import Prelude
>    hiding ( (.) , and , sum
>           , concat, foldr, curry
>           , uncurry, ($))
> import Data.Char


Funções de ordem superior
=========================

Função de ordem superior para mudar a
ordem de parâmetros.

> flip :: (a -> b -> c) -> b -> a -> c
> flip f y x = f x y

> curry :: ((a,b) -> c) -> a -> b -> c
> curry f x y = f (x,y)

> uncurry :: (a -> b -> c) -> (a,b) -> c
> uncurry f (x,y) = f x y 

> ($) :: (a -> b) -> a -> b
> f $ x = f x

Funções anônimas
=================

> doubleAll :: [Int] -> [Int]
> doubleAll xs = map (\ x -> 2 * x) xs

Currying e Aplicação parcial
============================

> add3 :: Int -> Int -> Int -> Int
> add3 = \ x y z ->  x + y + z

> test = add3 2 2

Definir a função
================

> applyAll :: [a -> a] -> a -> a
> applyAll []       x = x
> applyAll (f : fs) x
>    = applyAll fs (f x)


Composição
==========

> (.) :: (b -> c) -> (a -> b) -> a -> c
> g . f = \ x -> g (f x)

> applyAll' :: [a -> a] -> a -> a
> applyAll' []       = id
> applyAll' (f : fs) = applyAll' fs . f
