---
title: Introdução a linguagem Agda - Parte 3
author: Programação Funcional
date: Prof. Rodrigo Ribeiro
---

Números Naturais
================

<!--
```agda
module AgdaIntro3 where

open import Equality

data Bool : Set where
  true false : Bool

data ⊥ : Set where

¬_ : Set → Set
¬ A = A → ⊥

_∙_ : ∀ {A B C : Set} →
        (B → C)       →
        (A → B)       →
        (A → C)
g ∙ f = λ x → g (f x)

data _⊎_ (A B : Set) : Set where
  inl : A → A ⊎ B
  inr : B → A ⊎ B
```
-->

- Representação em Agda

```agda
data ℕ : Set where -- \BN
  zero : ℕ
  succ : ℕ → ℕ

{-# BUILTIN NATURAL ℕ #-}
```

Soma
====

```agda
infixl 6 _+_

_+_ : ℕ → ℕ → ℕ
zero + m = m
(succ n) + m = succ (n + m)
```

Soma
=====

Exemplo

```agda
test1 : 1 + 1 ≡ 2
test1 = refl
```

Soma
====

- No exemplo anterior, temos que uma
chamada de função é feita na assinatura
de tipo de `test1`.

- Tipo `1 + 1 ≡ 2`

Soma
====

- Congruência

```agda
cong : ∀ {A B : Set}{x y : A} →
         (f : A → B)          →
         x ≡ y                →
         f x ≡ f y
cong f refl = refl
```


Soma
====

- Um primeiro teorema provado por indução.
   - Indução em Agda: funções recursivas.
   
```agda
+-zero-r : ∀ (n : ℕ) → n + 0 ≡ n
+-zero-r zero = refl
+-zero-r (succ n)
   = begin
       succ n + 0     ≡⟨ refl ⟩
       succ (n + 0)   ≡⟨ cong succ (+-zero-r n) ⟩
       succ n
     ∎
```


Soma
====

- Exemplo: mais uma prova por indução.

```agda
+-succ : ∀ (n m : ℕ) → succ (n + m) ≡ n + succ m
+-succ zero m = refl
+-succ (succ n) m
  = begin
      succ (succ n + m)    ≡⟨ refl ⟩
      succ (succ (n + m))  ≡⟨ cong succ (+-succ n m) ⟩
      succ (n + succ m)    ≡⟨ refl ⟩
      succ n + succ m
    ∎
```

Soma
====

- Demonstrando a comutatividade da adição.

```agda
+-comm : ∀ (n m : ℕ) → n + m ≡ m + n
+-comm zero m = sym (+-zero-r m)
+-comm (succ n) m
   = begin
       succ n + m   ≡⟨ refl ⟩
       succ (n + m) ≡⟨ cong succ (+-comm n m) ⟩
       succ (m + n) ≡⟨ +-succ m n ⟩
       m + succ n
     ∎
```

Soma
====

- A sintaxe de Agda permite a definição
de operadores para construir provas
envolvendo igualdades.

- Usando esses operadores, podemos construir
provas similares às feitas "manualmente".

Soma
====

- Exercício: Prove que a soma é associativa.

```agda
+-assoc : ∀ {n m p : ℕ} →
            n + (m + p) ≡ (n + m) + p
+-assoc = {!!}
```

Exercício
=========

- Implemente uma função para multiplicação
de número naturais.

- Prove que sua função é comutativa.

Predicados
==========

- Definindo a relação de menor ou igual.

```agda
data _≤_ : ℕ → ℕ → Set where
  ≤-zero : ∀ {m} → 0 ≤ m
  ≤-succ : ∀ {n m} →
             n ≤ m →
             succ n ≤ succ m
```

Exemplos
========

- Provando que 3 ≤ 5.

```agda
test2 : 3 ≤ 5
test2 = ≤-succ
      ( ≤-succ
      ( ≤-succ ≤-zero))
```

Reflexiva
=========

- ≤ é uma relação reflexiva.

```agda
≤-refl : ∀ {n} → n ≤ n
≤-refl {zero} = ≤-zero
≤-refl {succ n'} = ≤-succ (≤-refl {n'})
```

Exercício
=========

- Prove que a relação ≤ é transitiva.

```agda
≤-trans : ∀ {n m p} →
            n ≤ m   →
            m ≤ p   →
            n ≤ p
≤-trans p1 p2 = {!!}
```

Predicados
==========

- A relação ≤ é total

```agda
data Total (n m : ℕ) : Set where
  n≤m : n ≤ m → Total n m
  m≤n : m ≤ n → Total n m
```

Predicados
==========

- A relação ≤ é total

```agda
≤-total : ∀ n m → Total n m
≤-total zero m = n≤m ≤-zero
≤-total (succ n) zero = m≤n ≤-zero
≤-total (succ n) (succ m) with ≤-total n m
... | n≤m p = n≤m (≤-succ p)
... | m≤n q = m≤n (≤-succ q)
```

Exercício
=========

- A relação ≤ é anti-simétrica.

```agda
≤-antisym : ∀ {n m} → n ≤ m → m ≤ n → n ≡ m
≤-antisym = {!!}
```

Predicado
=========

- Números pares.

```agda
data Even : ℕ → Set where
  zero : Even 0
  succ : ∀ {n} → Even n → Even (2 + n)
```

Predicado
=========

- Teorema simples.

```agda
even-succ-inv : ∀ {n} → Even (2 + n) → Even n
even-succ-inv (succ p) = p
```

Predicado
=========

- Decidibilidade

```agda
data Dec (P : Set) : Set where
  yes : P → Dec P
  no  : ¬ P → Dec P
```

Predicado
=========

- Even é decidível

```agda
even : ∀ n → Dec (Even n)
even zero = yes zero
even (succ zero) = no (λ ())
even (succ (succ n)) with even n
... | yes p = yes (succ p)
... | no q = no (q ∙ even-succ-inv)
```

Exercício
=========

- Prove o seguinte teorema sobre Even.

```agda
+-even : ∀ {n m} → Even n → Even m → Even (n + m)
+-even = {!!}
```

Exercício
=========

- Definir um predicado para números ímpares

```agda
data Odd : ℕ → Set where
```

Exercício
=========

- Provar o teorema

```agda
allN : ∀ (n : ℕ) → Even n ⊎ Odd n
allN n = {!!}
```
