> import Data.Char

Cifra de César
==============

Introdução
----------

- Algoritmo simples para criptografia
de texto

- Consiste em deslocar o texto em "n"
posições

- Exemplo: encode 2 "abc" == "cde"


Convertendo caracteres em inteiros
----------------------------------

> char2Int :: Char -> Int
> char2Int c = ord c - ord 'a'

> int2Char :: Int -> Char
> int2Char n = chr (ord 'a' + n)

Criptografando um caractere
---------------------------

> shift :: Int -> Char -> Char
> shift n c
>  | isLower c = int2Char (m `mod` 26)
>  | otherwise = c
>  where m = char2Int c + n

Criptografando um String
------------------------

> encode :: Int -> String -> String
> encode n = map (shift n)

Descriptografando
-----------------

> decode :: Int -> String -> String
> decode n = encode (- n)
