Avaliação lazy
==============

> module Aula7 where

> import StoreModel

Introdução
-----------

A avaliação de Haskell é bastante diferente
das estratégias utilizadas pela maioria das
linguagens de programação.

Haskell utiliza avaliação _lazy_ que significa
que apenas as subexpressões necessárias para
alcançar o resultado final são executadas e
isso é feito no último momento possível.

Por exemplo, na expressão

head [2 + 3 , 4 * 5]

a multiplicação não é executada, por não
relevante para o resultado final.

Outro exemplo, considere o seguinte par:

fst (1 , error "Some message")

a função error não é executada por não ser
necessária.

Apesar de parecer "mágica", a avaliação
_lazy_ pode ocasionar problemas de desempenho
se não utilizada de forma consciente.


Controle de brindes
-------------------

Retornando ao nosso exemplo
de loja, considere que a empresa
decidiu distribuir brindes únicos,
isto é numerados para os primeiros 100
clientes que efetuarem compras.
Para controlar esses brindes no sistema,
vamos definir o seguinte tipo de
dados:

> data Gift
>   = Gift {
>       name   :: Name
>     , unique :: Int
>     } deriving (Eq, Ord, Show)

Usando esse tipo, podemos retornar
todos os brindes de nossa loja
como uma lista:

> giftsFrom :: Int -> [Gift]
> giftsFrom n
>   = Gift (Name "Beer Store")
>          n
>     : giftsFrom (n + 1)

Veja que essa função não possui
caso base para a recursão. O que irá
acontecer ao executá-la é a criação
de uma lista infinita de valores do
tipo Gift, como a seguir:

> allGifts :: [Gift]
> allGifts = giftsFrom 1

> promotion :: [Gift]
> promotion = take 100 allGifts

Outro exemplo, podemos obter a
lista de todos os números da
sequência de Fibonacci da
seguinte forma:

> fibs :: [Int]
> fibs
>   = 0 : 1 : zipWith (+)
>                     fibs
>                     (tail fibs)

em que zipWith é a seguinte função

zipWith :: (a -> b -> c) ->
           [a]           ->
           [b]           ->
           [c]
zipWith _ [] _ = []
zipWith _ _ [] = []
zipWith f (x : xs) (y : ys)
   = f x y : zipWith f xs ys

que aplica uma função f a
elementos em posições iguais de
listas fornecidas como parâmetros.

Logo, a chamada a fibs é executada
da seguinte forma:

0 : 1 : zipWith (+)
                fibs
                (tail fibs)

==>

0 : 1 : zipWith (+)
                (0 : 1 : zipWith (+) fibs (tail fibs))
                (1 : zipWith (+) fibs (tail fibs))

==> 0 : 1 : 1 : zipWith (+)
                        (1 : zipWith (+) fibs (tail fibs))
                        (zipWith (+) fibs (tail fibs))

==> 0 : 1 : 1 : zipWith (+)
                        (1 : zipWith (+)
                                     (0 : 1 : zipWith (+)
                                                      fibs
                                                      (tail fibs))
                                     (1 : zipWith (+) fibs (tail fibs)))
                        (zipWith (+)
                                 (0 : 1 : zipWith (+) fibs (tail fibs))
                                 (1 : zipWith (+) fibs (tail fibs))

==>






Estudo de caso: Crivo de Eratóstenes
------------------------------------

O Crivo de Eratóstenes é um conhecido algoritmo para produzir
todos os números primos. Ele pode ser descrito pelos seguintes
passos:

1. Escreva a lista 2, 3, 4, ...

2. Marque o primeiro elemento como primo e chame-o de p

3. Remova todos os múltiplos de p da lista.

4. Retorne ao passo 2.

Idealmente, nosso código deve ser algo similar a:

> primes :: [Int]
> primes = sieve [2 ..]

A lista [2 ..] representa a lista de números iniciando em 2
e incrementando de 1 em 1 até o infinito.

> sieve :: [Int] -> [Int]
> sieve [] = []
> sieve (x : ys) = x : sieve [y | y <- ys, y `mod` x /= 0]

Com isso, podemos obter todos os primos menos que um
certo valor da seguinte forma:

> primesFrom :: Int -> [Int]
> primesFrom n = takeWhile (<= n) primes
