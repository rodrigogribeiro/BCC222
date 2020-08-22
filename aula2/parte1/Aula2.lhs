---
author: Programação Funcional
title: Tipos em Haskell
date: Prof. Rodrigo Ribeiro
---

Tipos em Haskell
================

- Haskell é uma linguagem fortemente tipada.

- Em programação funcional, tipos guiam como
devemos construir programas


Tipos em Haskell
================

- Nessa aula, veremos alguns tipos básicos
da linguagem Haskell.

- Objetivo é declarar o modelo de dados
de nossa aplicação de vendas de cervejas.


Caracteres
==========

- Representados pelo tipo Char

- Literais usando aspas simples

- Diversas funções presentes na biblioteca
Data.Char


Tipos Funcionais
================

```haskell
Data.Char*> :t toUpper

toUpper :: Char -> Bool
````

Tipos Funcionais
=================

Forma geral:

```haskell
T1 -> T2 -> ... Tn
```

Tipos Funcionais
================

- Parâmetros da função: T1, T2, ....

- Resultado: Tn

Tipos Funcionais
================

- Associam à direita.

```haskell
T1 -> T2 -> T3 -> T4 == T1 -> (T2 -> (T3 -> T4))
```

Números
=======

- Tipos Int, Integer, Float e Double.

- Int: inteiros nativos da arquitetura.

- Integer: inteiros de precisão arbitrária.

Números
=======

- Float e Double: ponto fluante de precisão simples
e dupla.

- Números racionais exatos: tipo Ratio.


Números
=======

- Em Haskell não há conversão implícita entre tipos numéricos.

- Existem diversas funções para realizar conversão entre tipos:
fromInteger, toInteger, fromFractional, toFractional, etc...

- Maiores detalhes sobre como lidar com números em Haskell serão
apresentados quando estudarmos sobrecarga.

Booleanos
=========

- Tipo de dados Bool: Valores True e False.

- Funções: &&, ||, not.


Listas
======

- Representação de sequências homogêneas (mesmo tipo) de valores.

- Listas são um tipo polimórfico: [a], em que _a_ é uma variável de tipo.


Listas
======

- Dois possíveis construtores de valores:
    - [] representa uma lista vazia
    - (x : xs) representa uma lista cujo primeiro
      elemento é _x_ e a cauda é _xs_.


Listas
======

![Representação de listas](list.png)

