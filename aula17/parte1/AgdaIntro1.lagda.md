---
title: Introdução a linguagem Agda - Parte 1
author: Programação Funcional
date: Prof. Rodrigo Ribeiro
---


Agda
====

- Linguagem de Programação com
suporte a tipos dependentes.

- Tipos dependentes: permitem
combinar programas e verificação
em um único formalismo.

Agda
====

- Sintaxe similar a de Haskell

```agda
module AgdaIntro1 where
```

Agda
====

- Tipos de dados: `Set` é o
tipo de todos os tipos.

```agda
data Bool : Set where
  true  : Bool
  false : Bool
```

Agda
====

- Uma simples função em Agda.

```agda
not : Bool → Bool
not true  = false
not false = true
```

Agda
====

- Definindo igualdade.

```agda
infix 4 _≡_

data _≡_ {A : Set}(x : A) : A → Set where
  refl : x ≡ x

{-# BUILTIN EQUALITY _≡_ #-}
```

Agda
====

- Igualdade

```agda
test1 : true ≡ true
test1 = refl

test2 : not true ≡ false
test2 = refl
```

Agda
====

- A linguagem Agda permite o
desenvolvimento incremental de
programas.

- Programas pode ser refinados
usando o conceito de _hole_


Agda
====

- Teorema: not é involutivo

```agda
not-inv : ∀ (b : Bool) → not (not b) ≡ b
not-inv b = {!!}
```

Agda
====

- Operadores lógicos

```agda
infixl 7 _∧_

_∧_ : Bool → Bool → Bool
true  ∧ b2 = b2
false ∧ _  = false
```

Agda
====

- A sintaxe de Agda permite
a definição de operadores
usando "_" para indicar parâmetros.

Agda
====

- Exemplo: prove que para todo b,
b ∧ true ≡ true

```agda
∧-true-r : ∀ (b : Bool) → b ∧ true ≡ b
∧-true-r b = {!!}
```

Agda
====

- Exercício: implemente a função de
disjunção ("ou") sobre booleanos.

```agda
infixl 6 _∨_

_∨_ : Bool → Bool → Bool
b1 ∨ b2 = {!!}
```

Agda
====

- Exercício: Prove os seguintes
resultados sobre booleanos.

```agda
deMorgan1 : ∀ (b b' : Bool) →
              not (b ∧ b') ≡ not b ∨ not b'
deMorgan1 b b' = {!!}

deMorgan2 : ∀ (b b' : Bool) →
              not (b ∨ b') ≡ not b ∧ not b'
deMorgan2 b b' = {!!}
```

Agda
====

- Prove os seguintes resultados.

```agda
or-false-l : ∀ (b : Bool) → false ∨ b ≡ b
or-false-l b = ?

or-false-r : ∀ (b : Bool) → b ∨ false ≡ b
or-false-r b = ?
```
