---
author: Programação Funcional
title: Definindo Funções
date: Prof. Rodrigo Ribeiro
---


Definindo Funções
=================

- Criar uma função para retornar o maior e o menor
elemento de uma lista não vazia de inteiros.

\begin{code}
maxmin :: [Int] -> (Int, Int)
maxmin xs
  = if null (tail xs) then (head xs, head xs)
    else (if (head xs) > fst (maxmin (tail xs))
          then head xs
          else fst (maxmin (tail xs))
         , if (head xs) < snd (maxmin (tail xs))
           then head xs
           else snd (maxmin (tail xs))
         )
\end{code}

\begin{code}
maxmin' :: [Int] -> (Int, Int)
maxmin' xs
  = if null tl then (h,h)
    else ( if h > max_xs then h else max_xs
         , if h < min_xs then h else min_xs)
  where -- bloco where
    h = head xs
    tl = tail xs
    p = maxmin' (tail xs)
    max_xs = fst p
    min_xs = snd p
\end{code}
