---
author: Programação Funcional
title: Funções Recursivas
date: Prof. Rodrigo Ribeiro
---

Setup
=====

> import Prelude
>   hiding ( sum, length, (++), replicate
>          , reverse)

Receita
=======

1. Defina o tipo da função.
2. Enumere os casos.
3. Defina os casos base.
4. Implemente os casos recursivos.

Somar elementos de uma lista
============================

> sum :: [Int] -> Int
> sum []       = 0           -- 1
> sum (x : xs) = x + sum xs  -- 2

x é a cabeça da lista / elemento
xs é a cauda da lista / lista

sum (1 : (2 : (3 : []))) ==> 2
1 + sum (2 : (3 : []))   ==> 2
    ------------------
1 + (2 + sum (3 : []))   ==> 2
         -----------
1 + (2 + (3 + sum []))   ==> 1
              ------
1 + (2 + (3 + 0)) == 6

Fatorial
========

> factorial :: Int -> Int
> factorial 0 = 1                     -- 1
> factorial n = n * factorial (n - 1) -- 2

factorial 3      ==>2
3 * factorial 2  ==>2
3 * (2 * factorial 1) ==>2
3 * (2 * (1 * factorial 0)) ==>1
3 * 2 * 1 * 1 == 6

Tamanho de uma lista
====================

> length :: [a] -> Int
> length [] = 0
> length (_ : xs) = 1 + length xs

Concatenação de listas
======================

> (++) :: [a] -> [a] -> [a]
> []       ++ ys = ys             -- 1
> (x : xs) ++ ys = x : (xs ++ ys) -- 2

(1 : 2 : []) ++ [3,4]   ==>2
1 : ((2 : []) ++ [3,4]) ==>2
     -----------------
1 : (2 : ([] ++ [3,4])) ==>1
          -----------
1 : 2 : [3,4] == [1,2,3,4]

Replicate
=========

> replicate :: Int -> a -> [a]
> replicate 0 v = []
> replicate n v = v : replicate (n - 1) v

Reverse
=======

> reverse :: [a] -> [a]
> reverse []       = []
> reverse (x : xs) = reverse xs ++ [x]

-- (++) é O(n)
-- reverse é O(n^2)

-- reverse [1,2,3]      ==>
-- reverse [2,3] ++ [1] ==>
-- (reverse [3] ++ [2]) ++ [1] ==>
-- ((reverse [] ++ [3]) ++ [2]) ++ [1] ==>
-- (([] ++ [3]) ++ [2]) ++ [1] ==>
-- ([3] ++ [2]) ++ [1] ==>
-- (3 : ([] ++ [2])) ++ [1] ==>
-- [3,2] ++ [1] ==>
-- 3 : ([2] ++ [1]) ==>
-- 3 : (2 : ([] ++ [1])) ==>
-- [3,2,1]

> reverse1 :: [a] -> [a]
> reverse1 xs = rev xs []
>    where
>      rev []       ac = ac
>      rev (x : xs) ac = rev xs (x : ac)

                    |_ pilha

reverse1 é O(n)






Inserir em uma lista ordenada
============================

> insert :: Int -> [Int] -> [Int]
> insert x []       = [x]
> insert x (y : ys)
>    | x <= y    = x : y : ys       -- 1
>    | otherwise = y : insert x ys  -- 2

Insertion sort
==============

> isort :: [Int] -> [Int]
> isort []       = []
> isort (x : xs) = insert x (isort xs)
