> module Review where

> import Data.Char
> import Prelude
>    hiding ( foldr
>           , length
>           , (++)
>           , map
>           , sum
>           , product
>           , or
>           , and
>           , filter
>           , all
>           , any
>           , takeWhile
>           , dropWhile
>           , words)

Revisão: foldr
==============

> foldr :: (a -> b -> b) ->
>          b             ->
>          [a]           ->
>          b
> foldr _ v []
>   = v
> foldr f v (x : xs)
>   = f x (foldr f v xs)

sum :: [Int] -> Int
sum [] = 0
sum (x : xs) = x + sum xs

foldr f v [1,2,3] =
foldr f v (1 : (2 : (3 : []))) =
             f    f    f  v

Esquema geral

foldr step base

step :: a -> b -> b
combinar a cabeça da lista
com o resultado das chamadas
recursivas de foldr.

base :: b
valor retornado quando alcançamos
a lista vazia

> sum :: [Int] -> Int
> sum
>   = foldr step base
>     where
>       base = 0
>       step x ac = x + ac

Exercício 1.

> product :: [Int] -> Int
> product = undefined


length :: [a] -> Int
length [] = 0
length (_ : xs) = 1 + length xs

> length :: [a] -> Int
> length
>   = foldr step base
>     where
>       base = 0
>       step _ ac = 1 + ac

Exercício 2.

or :: [Bool] -> Bool
or [] = False
or (x : xs) = x || or xs

> or :: [Bool] -> Bool
> or = undefined

Exercício 3.

and :: [Bool] -> Bool
and [] = True
and (x : xs) = x && and xs

> and :: [Bool] -> Bool
> and = undefined


filter :: (a -> Bool) ->
          [a]         ->
          [a]
filter _ [] = []
filter p (x : xs)
   | p x = x : filter p xs
   | otherwise = filter p xs

> filter :: (a -> Bool) ->
>           [a]         ->
>           [a]
> filter p
>   = foldr step base
>   where
>     base = []
>     step x ac
>      | p x = x : ac
>      | otherwise = ac

all :: (a -> Bool) ->
       [a]         ->
       Bool
all _ [] = True
all p (x : xs) = p x && all xs


> all :: (a -> Bool) ->
>        [a]         ->
>        Bool
> all p
>   = foldr step base
>     where
>       base = True
>       step x ac = p x && ac

Exercício 4.

any :: (a -> Bool) ->
       [a]         ->
       [a]
any _ [] = False
any p (x : xs) = p x || any p xs

> any :: (a -> Bool) ->
>        [a]         ->
>        [a]
> any = undefined


map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x : xs) = f x : map f xs

> map :: (a -> b) -> [a] -> [b]
> map f
>  = foldr step base
>    where
>      base = []
>      step x ac = f x : ac

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x : xs) ++ ys = x : (xs ++ ys)

> (++) :: [a] -> [a] -> [a]
> xs ++ ys
>   = foldr step base xs
>     where
>       base = ys
>       step x ac = x : ac



takeWhile :: (a -> Bool) ->
             [a]         ->
             [a]
takeWhile _ [] = []
takeWhile p (x : xs)
   | p x = x : takeWhile p xs
   | otherwise = []

> takeWhile :: (a -> Bool) ->
>              [a]         ->
>              [a]
> takeWhile p
>   = foldr step base
>     where
>       base = []
>       step x ac =
>         if p x
>         then x : ac
>         else ac

dropWhile :: (a -> Bool) ->
             [a]         ->
             [a]
dropWhile _ [] = []
dropWhile p (x : xs)
  = if p x
    then dropWhile p xs
    else x : xs

> dropWhile :: (a -> Bool) ->
>              [a]         ->
>              [a]
> dropWhile p
>    = fst . foldr step base
>      where
>       base = ([],[])
>       step x (xs,ys)
>        | p x = (xs, x : ys)
>        | otherwise = (x:ys,x:ys)


> words :: String -> [String]
> words
>   = uncurry (:) . foldr step base
>     where
>       base = ([], [])
>       step c (w, ws)
>         | isSpace c
>            = ([], w : ws)
>         | otherwise
>            = (c : w, ws)




> data Tree a
>   = Leaf
>   | Node a (Tree a) (Tree a)
>   deriving Show

> foldT :: (a -> b -> b -> b) ->
>          b                  ->
>          Tree a             ->
>          b
> foldT _ v Leaf = v
> foldT f v (Node v' l r)
>   = f v' (foldT f v l)
>          (foldT f v r)

> size :: Tree a -> Int
> size
>   = foldT step base
>    where
>      base = 0
>      step _ acl acr
>        = 1 + acl + acr

Exercício. altura de uma árvore.

> data Nat = Zero | Succ Nat
>    deriving Show

> fold :: (a -> a) ->
>         a        ->
>         Nat      ->
>         a
> fold _ v Zero = v
> fold f v (Succ n)
>   = f (fold f v n)

Zero .+. m = m
(Succ n) .+. m = Succ (n .+. m)

> (.+.) :: Nat -> Nat -> Nat
> n .+. m
>   = fold step m n
>     where
>       step ac = Succ ac

> toNat :: Int -> Nat
> toNat 0 = Zero
> toNat n = Succ (toNat (n - 1))
