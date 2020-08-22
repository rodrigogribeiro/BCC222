> import Prelude hiding (zip, repeat, take, iterate, drop)
> import Data.Char

Serialização
============

- Conversão de valores para / de sequências de bits

- Problema recorrente em software de sistemas:
pacotes de redes, formatos de arquivos, etc.


Novas Funções sobre Listas
==========================

* take n xs obtém os n primeiros elementos de uma lista

> take :: Int -> [a] -> [a]
> take 0 _  = []
> take n [] = []
> take n (x : xs) = x : take (n - 1) xs

* drop n xs remove os n primeiros elementos de uma lista

> drop :: Int -> [a] -> [a]
> drop 0 xs = xs
> drop n (_ : xs) = drop (n - 1) xs
> drop _ [] = error "drop: Empty List!"

* zip combina duas listas em uma lista de pares

> zip :: [a] -> [b] -> [(a,b)]
> zip [] _  = []
> zip _  [] = []
> zip (x : xs) (y : ys) = (x,y) : zip xs ys

* Gerar uma lista infinita de valores

> repeat :: a -> [a]
> repeat x = x : repeat x

* iterate f x gera uma lista infinita de
resultados de aplicar f a x repetidamente

> iterate :: (a -> a) -> a -> [a]
> iterate f x = x : iterate f (f x)

Serialização
============

> type Bit = Int

> bin2Int :: [Bit] -> Int
> bin2Int bs = sum [w * b | (w,b) <- zip weights bs]
>        where
>          weights = iterate (* 2) 1

1001

1 x 2^3 + 0 x 2 ^ 2 + 0 x 2 ^ 1 + 1 x 2 ^ 0

> int2Bin :: Int -> [Bit]
> int2Bin 0 = []
> int2Bin n = n `mod` 2 : int2Bin (n `div` 2)

9 mod 2 = 1 * [1, 0, 0, 1]
9 div 2 = 4
4 mod 2 = 0 *
4 div 2 = 2
2 mod 2 = 0 *
2 div 2 = 1
1 mod 2 = 1 *
1 div 2 = 0

Gerando Bytes
=============

> make8 :: [Bit] -> [Bit]
> make8 bs = take 8 (bs ++ repeat 0)

> chop8 :: [Bit] -> [[Bit]]
> chop8 [] = []
> chop8 bs = take 8 bs : chop8 (drop 8 bs)


Codificação de Strings
======================

> encode :: String -> [Bit]
> encode = concat . map (make8 . int2Bin . ord)
                        Char -> [Bit]
                    String -> [[Bit]]
           [Bit]

Decodificação de Strings
========================

> decode :: [Bit] -> String
> decode = map (chr . bin2Int) . chop8
                                 [Bit] -> [[Bit]]
                [Bit] -> Char
           [Char] == String
