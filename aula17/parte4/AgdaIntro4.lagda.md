---
title: Introdução a linguagem Agda - Parte 4
author: Programação Funcional
date: Prof. Rodrigo Ribeiro
---

Listas
======

<!--
```agda
module AgdaIntro4 where

open import Equality

data Bool : Set where
  true false : Bool

data ⊥ : Set where

data ℕ : Set where
  zero : ℕ
  succ : ℕ → ℕ

{-# BUILTIN NATURAL ℕ #-}


_+_ : ℕ → ℕ → ℕ
zero + m = m
(succ n) + m = succ (n + m)

¬_ : Set → Set
¬ A = A → ⊥

_∙_ : ∀ {A B C : Set} →
        (B → C)       →
        (A → B)       →
        (A → C)
g ∙ f = λ x → g (f x)

infixr 5 _∷_

```
-->

- Representação em Agda

```agda
data List (A : Set) : Set where
  []  : List A
  _∷_ : A → List A → List A
```

Listas
======

- Algumas funções.

```agda
length : ∀ {A : Set} → List A → ℕ
length [] = 0
length (_ ∷ xs) = 1 + length xs

_++_ : ∀ {A : Set} → List A → List A → List A
[] ++ ys = ys
(x ∷ xs) ++ ys = x ∷ (xs ++ ys)
```

Teorema
=======

- Prova usando operadores de igualdade.

```agda
++-length : ∀ {A : Set}
            (xs ys : List A) →
            length (xs ++ ys) ≡ length xs + length ys
++-length [] ys = refl
++-length (x ∷ xs) ys
  = begin
     length (x ∷ xs ++ ys)       ≡⟨ refl ⟩
     length (x ∷ (xs ++ ys))     ≡⟨ refl ⟩
     1 + length (xs ++ ys)       ≡⟨ cong succ
                                         (++-length xs ys) ⟩
     1 + (length xs + length ys) ≡⟨ refl ⟩
     (1 + length xs) + length ys ≡⟨ refl ⟩
     length (x ∷ xs) + length ys
    ∎
```
