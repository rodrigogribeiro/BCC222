---
author: Programação Funcional
title: Definindo Funções
date: Prof. Rodrigo Ribeiro
---

Definindo Funções
=================

- Funções são compostas por:
    - Nome
    - Parâmetros
    - Corpo
    - Assinatura de tipos (opcional)

Definindo Funções
=================

- Exemplo:
    - Definir uma função que, a partir de
      uma lista de strings, retorne a primeira
      string desta lista ou a string "empty"
      caso a lista fornecida seja vazia.

Definindo Funções
=================

- Para isso, usaremos as seguinets funções da
biblioteca Prelude de Haskell:

```haskell
null :: [a] -> Bool
head :: [a] -> a
tail :: [a] -> [a]
```

Definindo Funções
=================

- Nomes de funções: iniciam com letras minúsculas.
- Nomes de tipos e construtores de tipos: iniciam
com letras maiúsculas.

Definindo Funções
=================

- 1. Defina o tipo da função
- 2. Expresse o resultado em termos dos parâmetros.

Exemplo 1
=========

\begin{code}
firstOrEmpty :: [String] -> String
firstOrEmpty xs
   = if null xs then "empty"
     else head xs
\end{code}


Exemplo 2
=========

\begin{code}
size :: [Int] -> Int
size xs
  = if null xs then 0
    else 1 + size (tail xs)
\end{code}

Exemplo 3
=========

\begin{code}
(+++) :: [Int] -> [Int] -> [Int]
xs +++ ys
  = if null xs then ys
    else (head xs) : ((tail xs) +++ ys)
\end{code}

Exercícios
==========

1. Desenvolver uma função que
inverte uma lista.

\begin{code}
inverte :: [Int] -> [Int]
inverte = undefined
\end{code}

2. Desenvolver uma função que
soma os elementos de uma lista
de números inteiros.

\begin{code}
soma :: [Int] -> Int
soma = undefined
\end{code}
