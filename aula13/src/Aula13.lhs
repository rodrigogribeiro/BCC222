---
title: Teste baseado em propriedades
author: Programação Funcional
date: Prof. Rodrigo Ribeiro
---


Setup Inicial
=============

> module Aula13 where

> import qualified Data.List as List
> import Test.QuickCheck


Teste baseado em propriedades
=============================

- Objetivo: mostrar como o mecanismo
de classes de tipos pode ser utilizado para
automatizar o processo de teste.

- QuickCheck: desenvolvido no ano 2000 e
utilizado em diversos projetos. Inclusive
para encontrar bugs em software de
telecom.


Benefícios do QuickCheck
=============================

- O desenvolvedor é forçado a pensar no que o código _deveria_ fazer;
- O QuickCheck encontra _corner-cases_ que ocasionam correções na
  especificação ou no código.
- Especificações funcionam como uma documentação verificável pelo compilador.

Um exemplo simples
===================

- Uma propriedade da inversão de listas é que esta deve ser involutiva.

> prop_revInv :: [Int] -> Bool
> prop_revInv xs = reverse (reverse xs) == xs


Um exemplo simples
==================

- Executamos o teste chamando a função quickCheck:

~~~~~{.haskell}
quickCheck prop_revInv
+++ OK, passed 100 tests.
~~~~~

Outro exemplo envolvendo listas
===============================

- Uma propriedade envolvendo reverse e ++:

> prop_revApp :: [Int] -> [Int] -> Bool
> prop_revApp xs ys
>   = reverse (xs ++ ys) == reverse xs ++ reverse ys

Outro exemplo envolvendo listas
===============================

- Porém...

~~~~~{.haskell}
*** Failed! Falsifiable (after 4 tests and 6 shrinks):
[0]
[1]
~~~~~~~


Corrigindo a propriedade
=========================

> prop_revOk :: [Int] -> [Int] -> Bool
> prop_revOk xs ys
>   = reverse (xs ++ ys) == reverse ys ++ reverse xs

- Agora, tudo ok...

~~~~~~{.haskell}
quickCheck prop_revOk
+++ OK, passed 100 tests.
~~~~~~~

Quantidade de testes
====================

> quickCheckN :: Testable prop => Int -> prop -> IO ()
> quickCheckN n
>   = quickCheckWith $
>       stdArgs { maxSuccess = n }

Quantidade de testes
====================

- Com isso, conseguimos:

~~~~~~{.haskell}
quickCheckN 1000 prop_revOk
+++ OK, passed 1000 tests.
~~~~~~~

Exercício
=========

- Outra propriedade atendida pela
função `reverse` é que essa
preserva o tamanho da lista
de entrada. Implemente essa
propriedade e use o QuickCheck
para testá-la.



Um exemplo um pouco maior
========================

> qsort :: Ord a => [a] -> [a]
> qsort [] = []
> qsort (x:xs)
>    = qsort lts ++ [x] ++ qsort gts
>      where
>        lts = [y | y <- xs, y <= x]
>        gts = [y | y <- xs, y > x]


Uma propriedade para ordenação
==============================

> isSorted :: [Int] -> Bool
> isSorted [] = True
> isSorted [ _ ] = True
> isSorted (x : x' : xs)
>    = x <= x' && isSorted (x' : xs)

Definindo uma propriedade para qsort
==============================

> prop_qsortSorted :: [Int] -> Bool
> prop_qsortSorted xs = isSorted (qsort xs)


QSort
=====

- So far, so good...

~~~~~{.haskell}
quickCheckN 10000 prop_qsortSorted
+++ OK, passed 10000 tests.
~~~~~

Idempotência
===========

> prop_qsortIdem :: [Int] -> Bool
> prop_qsortIdem xs
>   = qsort (qsort xs) == qsort xs

- Novamente, sem problemas...

~~~~~{.haskell}
quickCheckN 10000 prop_qsortIdem
+++ OK, passed 10000 tests.
~~~~~~~~

Mínimo
======

> prop_headQsortMin :: [Int] -> Bool
> prop_headQsortMin xs
>   = head (qsort xs) == minimum xs

- Porém, obtemos o seguinte erro...

~~~~~~{.haskell}
quickCheck prop_headQsortMin
*** Failed! Exception: 'Prelude.head: empty list' (after 1 test):
[]
~~~~~~~~

Problema
========

- A propriedade anterior não exclui listas vazias.
- Usamos o operador ==> para excluir casos de teste que
não satisfazem uma certa condição.

> prop_headQsortMin1 :: [Int] -> Property
> prop_headQsortMin1 xs
>   = not (null xs) ==> head (qsort xs) == minimum xs

Resultado
=========

- Teste executado com sucesso. 18 listas vazias foram
geradas e descartadas.

~~~~~{.haskell}
*Main> quickCheck prop_headQsortMin1
+++ OK, passed 100 tests; 18 discarded.
~~~~~~

Teste usando um modelo
======================

- Outra forma de usar o QuickCheck é para testar uma
implementação (possivelmente mais eficiente) contra outra
conhecidamente correta (porém, não eficiente)

> prop_qsort_sort :: [Int] -> Bool
> prop_qsort_sort xs
>   = qsort xs == List.sort xs

~~~~~~{.haskell}
*** Failed! Falsifiable (after 5 tests):
[-3,-3]
~~~~~~~

Geradores de dados
=============================

- Ao invés de usar condições, podemos
gerar dados que as atendam.

- O QuickCheck possui uma série de
funções para auxiliar a criação
de valores aleatórios.

- Para isso, o QuickCheck usa a
mônada Gen.


Algumas funções do QuickCheck
=============================

- Escolher valores aleatórios em um intervalo.

~~~~{.haskell}
choose :: Random a => (a,a) -> Gen a
~~~~~~

- Escolher um valor a partir de uma lista

~~~~~{.haskell}
elements :: [a] -> Gen a
~~~~~

Algumas funções do QuickCheck
=============================

- Escolher um gerador a partir de uma lista

~~~~~{.haskell}
oneof :: [Gen a] -> Gen a
~~~~~~

- Escolher um gerador, considerando pesos

~~~~{.haskell}
frequency :: [(Int , Gen a)] -> Gen a
~~~~~

Classe Arbitrary
================

- Classe de tipos para geradores.

~~~~{.haskell}
class Arbitrary a where
   arbitrary :: Gen a
~~~~~


Estudo de caso: Gerando listas ordenadas
=================================

- Gerando listas

> genList :: Arbitrary a => Gen [a]
> genList
>   = oneof [ return []
>           , (:) <$> arbitrary <*> genList ]

- Porém, isso está gerando muitas listas vazias.


Gerador de listas, nova versão
==============================

> genList1 :: Arbitrary a => Gen [a]
> genList1
>   = frequency [ (1, return [])
>               , (99, (:) <$> arbitrary <*> genList1) ]

- Agora, geramos listas muito grandes! Isso
pode dificultar a depuração!

Gerador de listas, nova versão
==============================

- Vamos usar o combinador sized:

~~~~~~{.haskell}
sized :: (Int -> Gen a) -> Gen a
~~~~~~~

que recebe um gerador que utiliza um parâmetro de tamanho
sobre suas entradas. Com isso, o QuickCheck gera entradas
crescentes.

Gerador de listas
=================

> genList2 :: Arbitrary a => Gen [a]
> genList2 = sized gen
>   where
>     gen n = frequency [ (1, return [])
>                       , (n, (:) <$> arbitrary <*>
>                                     gen (n `div` 2))]

Gerador de listas, nova versão
=============================

- Mas, como gerar listas ordenadas?
- Simples, agora geramos listas menores, podemos ordená-las!

> genList3 :: (Arbitrary a, Ord a) => Gen [a]
> genList3 = List.sort <$> genList2


Usando newtype
==============

> newtype SortList a
>    = SortList { out :: [a]} deriving (Eq, Ord, Show)

> genSortList :: (Arbitrary a, Ord a) => Gen (SortList a)
> genSortList = SortList <$> genList3

Testando o gerador
==================

> prop_genSortListSorted :: Property
> prop_genSortListSorted
>    = forAll (genSortList :: Gen (SortList Int))
>             (\ (SortList xs) -> isSorted xs)


Retornando ao quicksort
=======================

> prop_qsort_sort2 :: Property
> prop_qsort_sort2
>    = forAll (genSortList :: Gen (SortList Int))
>             (\ (SortList xs) -> qsort xs == List.sort xs)


Elementos distintos
===================

> distinct :: [Int] -> Bool
> distinct [] = True
> distinct (x : xs)
>    = x `notElem` xs && distinct xs


> prop_qsortDistinct :: [Int] -> Property
> prop_qsortDistinct xs
>   = distinct xs ==> qsort xs == List.sort xs

Executando os testes
====================

> test_qsort :: IO ()
> test_qsort
>    = do
>        putStrLn "Testing cases for qsort"
>        quickCheckN 1000 prop_qsort_sort2
>        quickCheckN 1000 prop_qsortDistinct

Code Coverage
=============

- Ferramenta para "medir" efetividade de
testes.

- Determina quais partes do código foram ou não
exercitadas pelos testes.

Code Coverage
=============

- Como fazer isso? Simples, basta executar

```
stack build --coverage
stack test  --coverage
stack hpc report aula13
```

Code Coverage
=============

- Resultados para `qsort`: 100% de cobertura!
   - Maior cobertura: maior garantia de correção!

```
Generating combined report
100% expressions used (81/81)
100% boolean coverage (2/2)
     100% guards (0/0)
     100% 'if' conditions (0/0)
     100% qualifiers (2/2)
100% alternatives used (5/5)
100% local declarations used (3/3)
100% top-level declarations used (9/9)
```

Code Coverage
=============

- Ferramenta gera um relatório HTML.

![Relatório de cobertura](coverage.png)

Estudo de caso
==============

- Verificar invariantes de árvores binárias
de busca.

> data Tree a
>   = Leaf
>   | Node a (Tree a) (Tree a)
>   deriving (Eq, Ord, Show)

Propriedade
===========

- Uma árvore é uma árvore de busca se:
1. Todos os valores à esquerda são menores que o valor atual.
2. Todos os valores à direita são maiores que o valor atual.

Propriedade
===========

> instance Foldable Tree where
>    foldr _ v Leaf = v
>    foldr f v (Node x l r)
>      = foldr f (f x (foldr f v r)) l

Propriedade
===========

> bst :: Ord a => Tree a -> Bool
> bst Leaf = True
> bst (Node x l r) = (all (<= x) l) && (all (>= x) r)

```haskell
all :: Foldable f => (a -> Bool) -> f a -> Bool
```

Insert
======

> insert :: Ord a => a -> Tree a -> Tree a
> insert x Leaf
>   = Node x Leaf Leaf
> insert x (Node y l r)
>     | x == y = Node y l r
>     | x < y  = Node y (insert x l) r
>     | x > y  = Node y l (insert x r)

Gerando árvores
===============

> genTree :: Int -> Gen (Tree Int)
> genTree n
>    | n <= 1 = return Leaf
>    | otherwise
>         = frequency
>             [
>                (20, return Leaf)
>             ,  (80, Node <$> (arbitrary :: Gen Int) <*>
>                              genTree n1 <*> genTree n1)
>             ]
>             where
>               n1 = n `div` 2

Propriedade
===========

> insert_BST :: Property
> insert_BST
>   = forAll (choose (5,10))
>            (\ n -> forAll (genTree n)
>                      (\ t -> forAll (arbitrary :: Gen Int)
>                            (\ x -> bst t ==> bst (insert x t))))

Resultado
=========

~~~~{.haskell}
*Main> quickCheckN 1000 insert_BST
+++ OK, passed 1000 tests; 1566 discarded.
~~~~~~

Resultado
=========

- Problema: Mais casos de testes descartados
do que utilizados.

- Motivo: Árvores geradas não atendem o
critério de árvores de busca.

- Solução: Gerar árvores de busca.

Gerar árvores
=============

> fromList :: Ord a => [a] -> Tree a
> fromList = foldr insert Leaf

> genTree2 :: Gen (Tree Int)
> genTree2 = (fromList . out) <$> genSortList


Testando o gerador
==================

> genTree2_ok :: Property
> genTree2_ok
>    = forAll genTree2 bst

Resultado
=========

~~~~~~{.haskell}
*Main> quickCheckN 1000 genTree2_ok
+++ OK, passed 1000 tests.
~~~~~~

Propriedade
===========

> insert_BST2 :: Property
> insert_BST2
>   = forAll genTree2
>            (\ t -> forAll (arbitrary :: Gen Int)
>                           (\ x -> bst t ==> bst (insert x t)))

Resultado
=========

~~~~~{.haskell}
quickCheckN 1000 insert_BST2
+++ OK, passed 1000 tests.
~~~~~~

Code Coverage
=============

- Resultados

```
100% expressions used (152/152)
 80% boolean coverage (4/5)
      66% guards (2/3), 1 always True
     100% 'if' conditions (0/0)
     100% qualifiers (2/2)
100% alternatives used (13/13)
100% local declarations used (3/3)
 94% top-level declarations used (16/17)
```

Code Coverage
=============

- Por que não obtivemos 100% de cobertura?

- Lembre-se: geramos apenas listas ordenadas e
usamos isso para gerar árvores de busca.

Code Coverage
=============

- Ferramenta gera um relatório HTML.
   - Listas ordenadas são a causa!

![](always-true.png){width=550}


Exercício
=========

- Codifique propriedades para
testar se as operações de inserção e
remoção de árvores binárias preservam
o invariante de árvores de busca.

Exercício
=========

- Elabore propriedades que relacionam
a operação de busca de elementos e as
operações de inserção / remoção de elementos.
Qual o resultado de seus testes?


Compilador
==========

- Voltemos ao exemplo do compilador da máquina de pilha.

- Usaremos o QuickCheck para validar a correção da
implementação

Expressões
==========

- Sintaxe de expressões

> data Exp
>   = Const Int
>   | Exp :+: Exp
>     deriving (Eq, Ord, Show)


Interpretador
=============

> eval :: Exp -> Int
> eval (Const n) = n
> eval (e :+: e')
>   = eval e + eval e'

Instruções
==========

> data Instr
>    = Push Int
>    | Add
>    deriving (Eq, Ord, Show)

> type Code = [Instr]
> type Stack = [Int]

Interpretador
=============

> exec :: Code -> Stack -> Stack
> exec [] s = s
> exec ((Push n) : c) s = exec c (n : s)
> exec (Add : c) (n : m : s) = exec c (n + m : s)

Compilador
==========

> compile :: Exp -> Code
> compile (Const n)
>   = [Push n]
> compile (e :+: e')
>   = compile e' ++ compile e ++ [Add]

Gerador
=======

> instance Arbitrary Exp where
>    arbitrary = sized genExp
>      where
>        genExp n
>          | n <= 1 = Const <$> arbitrary
>          | otherwise
>             = frequency
>                  [
>                    (30, Const <$> arbitrary)
>                  , (70, (:+:) <$> genExp n2 <*> genExp n2)]
>               where n2 = n `div` 2

Propriedade
===========

> compileOk :: Exp -> Stack -> Bool
> compileOk e s
>   = eval e : s == exec (compile e) s


Resultado
=========

~~~~~{.haskell}
*Main> quickCheckN 1000 compileOk
+++ OK, passed 1000 tests.
~~~~~~

Code Coverage
=============

```
Generating combined report
100% expressions used (219/219)
 71% boolean coverage (5/7)
      60% guards (3/5), 2 always True
     100% 'if' conditions (0/0)
     100% qualifiers (2/2)
100% alternatives used (22/22)
100% local declarations used (5/5)
 87% top-level declarations used (21/24)
```

The truth...
============

- Primeira versão do interpretador

~~~~{.haskell}
exec :: Code -> Stack -> Stack
exec [] s = s
exec ((Push n) : c) s = n : s
exec (Add : c) (n : m : s) = n + m : s
~~~~~~~

Resultado
=========

~~~~~{.haskell}
*Main> quickCheck compileOk
*** Failed! Falsified (after 4 tests and 1 shrink):
Const (-2) :+: Const 0
[]
~~~~~~~

Bug!
====

- Função `exec` não era executada recursivamente...

Lição
=====

- Testes baseados em propriedades são uma forma fácil
de garantir qualidade do seu código.

- Praticamente todas as linguagens possuem um equivalente
ao QuickCheck para testes: Java, JavaScript, C, C++, PHP, etc...

- Testes só fazem sentido quando acompanhados por resultado
de cobertura para medir sua eficácia.
