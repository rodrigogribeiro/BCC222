---
title: Introdução a linguagem Agda - Parte 2
author: Programação Funcional
date: Prof. Rodrigo Ribeiro
---

Agda
====

<!--
```agda
module AgdaIntro2 where

open import Equality

data Bool : Set where
  true false : Bool
```
-->

- Agda é capaz de representar provas
da lógica formal usando tipos de dados.

- Provas são automaticamente verificadas
pelo compilador.

Lógica
======

- Usaremos tipos para representar conjuntos
de provas de uma certa proposição.

Lógica
======

- Verdadeiro: Conjunto com um único elemento

- Falso: conjunto vazio

```agda
data ⊤ : Set where -- \top
  tt : ⊤

data ⊥ : Set where -- \bot
```

Lógica
======

- Negação

```agda
¬_ : Set → Set
¬ A = A → ⊥

absurd : {A : Set} → ⊥ → A
absurd () -- absurd pattern
```

Lógica
======

- Conjunção      

```agda
data _∧_ (A B : Set) : Set where
  _,_ : A → B → A ∧ B

fst : ∀ {A B} → A ∧ B → A
fst (x , _) = x

snd : ∀ {A B} → A ∧ B → B
snd (_ , y) = y
```

Lógica
======

- Disjunção

```agda
data _∨_ (A B : Set) : Set where
  inl : A → A ∨ B
  inr : B → A ∨ B

∨-elim : ∀ {A B C : Set} → A ∨ B   →
                           (A → C) →
                           (B → C) → C
∨-elim (inl a) f g = f a
∨-elim (inr b) f g = g b
```

Lógica
======

- Implicação lógica é representada
como funções!

```agda
modus-ponens : ∀ {A B : Set} → (A → B) → A → B
modus-ponens f x = f x
```

Lógica
======

- Composição de funções como um teorema.

```agda
_∙_ : ∀ {A B C : Set} →
        (B → C)       →
        (A → B)       →
        (A → C)
g ∙ f = λ x → g (f x)
```

Lógica
======

- Equivalência lógica.

```agda
record _↔_ (A B : Set) : Set where
  field
    to   : A → B
    from : B → A
```

Lógica
======

- Exemplo:

```agda
∧-comm : ∀ {A B : Set} →
         (A ∧ B) ↔ (B ∧ A)
∧-comm
   = record {
       to = swap
     ; from = swap }
  where
    swap : ∀ {A B : Set} →
            A ∧ B         →
            B ∧ A
    swap (a , b) = b , a
```

Lógica
======

- Exercício: Prove a equivalência.

```agda
∨-comm : ∀ {A B : Set} →
           (A ∨ B) ↔ (B ∨ A)
∨-comm = {!!}
```

Lógica
======

- Quantificador existencial:

```agda
record Σ (A : Set)(B : A → Set) : Set where
  constructor _,_
  field
    witness : A
    the-proof : B witness
```

Lógica
======

- Exemplo

```agda
dummy : Σ Bool (λ b → b ≡ true)
dummy = true , refl
```

Ctrl-c Ctr-r
