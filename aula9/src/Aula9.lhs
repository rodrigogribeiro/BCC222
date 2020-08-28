---
author: Programação Funcional
title: Estudo de caso - Expressões.
date: Prof. Rodrigo Ribeiro
---


Setup
=====

> module Aula9 where

> tODO :: a
> tODO = undefined

Expressões
==========

- Vamos implementar diversas funções
sobre expressões aritméticas.

- Ao acrescentar novas funcionalidades,
vamos usar mônadas para simplificar
a implementação.

Expressões
==========

- Além disso, vamos introduzir o
conceito de functores aplicativos
e como esses permitem modificar
a semântica de chamadas de função.


Expressões - V1
===============

- Sintaxe de expressões

> data Exp1
>   = Const1 Int
>   | Add1 Exp1 Exp1
>   | Mul1 Exp1 Exp1
>   deriving (Eq, Ord, Show)

Interpretador - V1
==============

> eval1 :: Exp1 -> Int
> eval1 (Const1 n)
>    = n
> eval1 (Add1 e1 e2)
>    = eval1 e1 + eval1 e2
> eval1 (Mul1 e1 e2)
>    = eval1 e1 * eval1 e2

Exercício
=========

- Implemente uma função
que converta expressões em
strings legíveis. Por exemplo,
Mul1 (Const1 2) (Add1 (Const1 1) (Const 3))
deve ser impresso como 2 * (1 + 3).

> pprint :: Exp1 -> String
> pprint = tODO

Exercício
=========

- Inclua a operação de subtração
na sintaxe de expressões. Modifique
sua implementação do interpretador
e da função de conversão para
string para refletir essa modificação.

Divisão
=======

- Considere a tarefa de incluir
a operação de divisão à sintaxe de
expressões.

Sintaxe
=======

> data Exp2
>   = Const2 Int
>   | Add2 Exp2 Exp2
>   | Mul2 Exp2 Exp2
>   | Sub2 Exp2 Exp2
>   | Div2 Exp2 Exp2
>   deriving (Eq, Ord, Show)

Interpretador
=============

- A divisão é uma função parcial:
Não está definida se o segundo
operando avaliar para zero.

- Para contornar esse caso, vamos
usar o tipo Maybe.
   - Nothing: representa que aconteceu
     uma divisão por zero.

Intepretador - V2
=================

> eval2 :: Exp2 -> Maybe Int
> eval2 (Const2 n)
>   = Just n
> eval2 (Add2 e1 e2)
>   = case (eval2 e1, eval2 e2) of
>       (Just n1, Just n2) -> Just (n1 + n2)
>       _                  -> Nothing

Interpretador
=============

> eval2 (Sub2 e1 e2)
>   = case (eval2 e1, eval2 e2) of
>       (Just n1, Just n2) -> Just (n1 - n2)
>       _                  -> Nothing

Interpretador
=============

> eval2 (Mul2 e1 e2)
>   = case (eval2 e1, eval2 e2) of
>       (Just n1, Just n2) -> Just (n1 * n2)
>       _                  -> Nothing

Intepretador
============

> eval2 (Div2 e1 e2)
>   = case eval2 e1 of
>       Just n1 ->
>         case eval2 e2 of
>           Just n -> if n == 0
>                     then Nothing
>                     else Just (n1 `div` n)
>           _      -> Nothing
>       _       -> Nothing


Interpretador
=============

- Apesar de simples, esse código
possui problemas:
    - Repetição na avaliação de soma,
      multiplicação e subtração.
    - Casamento de padrão aninhado
      na equação de divisão.

Problema 1
==========

- Repetição de código de soma, subtração
e multiplicação.

> evalOp :: (Int -> Int -> Int)
>        -> Maybe Int
>        -> Maybe Int
>        -> Maybe Int
> evalOp op (Just v1) (Just v2)
>   = Just (op v1 v2)
> evalOp _ _ _
>   = Nothing

Problema 2
==========

- Casamentos de padrão aninhados sobre o tipo Maybe
são facilmente resolvidos pela Monad do tipo Maybe.

> evalDiv :: Exp2 -> Exp2 -> Maybe Int
> evalDiv e1 e2
>    = do
>        v1 <- eval21 e1
>        v2 <- eval21 e2
>        return (v1 `div` v2)

Interpretador
=============

> eval21 :: Exp2 -> Maybe Int
> eval21 (Const2 n)
>    = Just n
> eval21 (Add2 e1 e2)
>    = evalOp (+) (eval21 e1)
>                 (eval21 e2)
> eval21 (Mul2 e1 e2)
>    = evalOp (*) (eval21 e1)
>                 (eval21 e2)
> eval21 (Sub2 e1 e2)
>    = evalOp (-) (eval21 e1)
>                 (eval21 e2)
> eval21 (Div2 e1 e2)
>    = evalDiv e1 e2

Problemas
=========

- Ainda temos muita repetição de
código.

- Podemos generalizar toda a definição
usando a função evalOp e mônadas.

Interpretador
=============

> evalOp1 :: (Int -> Int -> Int)
>         -> Exp2
>         -> Exp2
>         -> Maybe Int
> evalOp1 op e1 e2
>    = do
>        v1 <- eval22 e1
>        v2 <- eval22 e2
>        return (v1 `op` v2)

Interpretador
=============

> eval22 :: Exp2 -> Maybe Int
> eval22 (Const2 n)
>    = Just n
> eval22 (Add2 e1 e2)
>    = evalOp1 (+) e1 e2
> eval22 (Sub2 e1 e2)
>    = evalOp1 (-) e1 e2
> eval22 (Mul2 e1 e2)
>    = evalOp1 (*) e1 e2
> eval22 (Div2 e1 e2)
>    = evalOp1 div e1 e2


Problemas
=========

- Versão atual é mais legível.

- Porém, a recursão mútua entre
evall22 e evalOp1 não é o ideal.

- Há como resolver?

evalOp1
=======

- A função evalOp1 aplica uma
função de tipo

```haskell
Int -> Int -> Int
```

a parâmetros de tipo

```haskell
Maybe Int
```

evalOp1
=======

- O ideal seria "transformar"
a função de tipo

```haskell
Int -> Int -> Int
```

em

```haskell
Maybe Int -> Maybe Int -> Maybe Int
```

para simplesmente aplicar o operador
aos resultados do interpretador.

Solução
=======

- Para entender a solução, vamos nos restringir
a funções de um parâmetro:

```haskell
g :: Int -> Int

v :: Maybe Int
```

Solução
=======

- Desejamos aplicar a função

```haskell
g :: Int -> Int
```

ao valor

````haskell
v :: Maybe Int
````

e produzir um valor de tipo Maybe Int

Functores!
=========

- Para resolver isso, basta usar fmap!

```haskell
fmap  :: (a   -> b  ) -> f      a  -> f     b

         (Int -> Int) -> Maybe Int -> Maybe Int
```

Functores!
==========

- Dessa forma, temos:

```haskell
g :: Int -> Int
v :: Maybe Int
g <$> v :: Maybe Int
```

- Para funções de um parâmetro, a solução funciona.
Mas, e funções com mais parâmetros?

Problema
========

- Vamos considerar a função

```haskell
g :: Int -> Int -> Int
```

e dois valores de tipo Maybe Int:

```haskell
v1 :: Maybe Int; v2 :: Maybe Int
````

Como aplicar g a v1 e v2?

Functores?
==========

- O que acontece ao usar fmap f?

```haskell
fmap :: (a    ->  b          ) -> f a -> f b
g    ::  Int  -> (Int -> Int))
``` 

Temos que fmap g teria o tipo

```haskell
fmap g :: f Int -> f (Int -> Int)
```

Functores
=======

- Fazendo f = Maybe, temos:

```haskell
fmap g :: Maybe Int -> Maybe (Int -> Int)
```

Aplicando a v1 :: Maybe Int, temos:

```haskell
fmap g v1 :: Maybe (Int -> Int)
```

Functores
=========

- Para finalizar, agora temos que combinar:

```haskell
fmap g v1 :: Maybe (Int -> Int)
```

e

```haskell
v2 :: Maybe Int
```

Aplicação?
==========

- Veja que os tipos

```haskell
Maybe (Int -> Int) e Maybe Int
```

correspondem a tipos válidos para
uma aplicação de função, exceto pelo
fato de estarem em um tipo de dados
Maybe.

Solução
=======

- Precisamos de uma abstração que
permita aplicar funções armazenadas
em construtores de tipos, como Maybe.

- Essa abstração é chamada de
functor aplicativo.

Applicative
===========

```haskell
class Functor f => Applicative f where
   pure  :: a -> f a
   (<*>) :: f (a -> b) -> f a -> f b
```

Applicative Maybe
=================

```haskell
instance Applicative Maybe where
  pure = Just
  (Just f) <*> (Just x) = Just (f x)
  _        <*> _        = Nothing
```

Exercício
=========

- Considere o seguinte tipo para representar
a mônada de estado:

> newtype State s a
>   = State {runState :: s -> (a,s)}

Implemente uma instância de Applicative para
o tipo State.

Solução
=======

Usando as funções de Applicative, podemos
aplicar a função g aos valores v1 e v2 usando:

```haskell
g :: Int -> Int -> Int
v1 ,v2 :: Maybe Int

g <$> v1 <*> v2 :: Maybe Int
```

Resumo
======

- A classe Applicative apresenta funções que
modificam o comportamento de chamada de função.

- Especialmente, o operador <*>, permite combinar
computações em uma estrutura de dados sem a
necessidade de casamento de padrão.

Interpretador
=============

> evalApp :: Exp2 -> Maybe Int
> evalApp (Const2 n)
>   = pure n
> evalApp (Add2 e1 e2)
>   = (+) <$> evalApp e1 <*> evalApp e2
> evalApp (Mul2 e1 e2)
>   = (*) <$> evalApp e1 <*> evalApp e2
> evalApp (Sub2 e1 e2)
>   = (-) <$> evalApp e1 <*> evalApp e2
> evalApp (Div2 e1 e2)
>   = evalDiv e1 e2
