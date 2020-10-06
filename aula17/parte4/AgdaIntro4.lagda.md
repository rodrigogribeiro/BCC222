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

infixl 6 _⊎_

data _⊎_ (A B : Set) : Set where
  inl : A → A ⊎ B
  inr : B → A ⊎ B

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

Exercício
=========

- Prove que a operação de concatenação de listas é associativa.

```agda
++-assoc : ∀ {A : Set}
             (xs ys zs : List A) →
             (xs ++ ys) ++ zs ≡ xs ++ (ys ++ zs)
++-assoc xs ys zs = {!!}
```

Exemplo
=======

- Função map

```agda
map : ∀ {A B : Set} → (A → B) → List A → List B
map f [] = []
map f (x ∷ xs) = f x ∷ map f xs

id : ∀ {A : Set} → A → A
id x = x
```

Exemplo
=======

- Teorema

```agda
map-id : ∀ {A : Set}(xs : List A) → map id xs ≡ xs
map-id [] = refl
map-id (x ∷ xs)
  = begin
     map id (x ∷ xs)  ≡⟨ refl ⟩
     id x ∷ map id xs ≡⟨ cong (_∷_ x) (map-id xs) ⟩
     id x ∷ xs        ≡⟨ refl ⟩
     x ∷ xs
    ∎
```

Map Fusion
==========

- Teorema

```agda
map-fusion : ∀ {A B C : Set}
               (g : B → C)
               (f : A → B)
               (xs : List A) →
               map (g ∙ f) xs ≡ (map g ∙ map f) xs
map-fusion g f [] = refl
```

Map Fusion
==========

- Continuação

```agda
map-fusion g f (x ∷ xs)
  = begin
      map (g ∙ f) (x ∷ xs)             ≡⟨ refl ⟩
      ((g ∙ f) x) ∷ map (g ∙ f) xs     ≡⟨ refl ⟩
      ((g ∙ f) x) ∷ map (g ∙ f) xs     ≡⟨ cong (_∷_ ((g ∙ f) x))
                                               (map-fusion g f xs) ⟩
      ((g ∙ f) x) ∷ (map g ∙ map f) xs ≡⟨ refl ⟩
      g (f x) ∷ (map g (map f xs))     ≡⟨ refl ⟩
      map g ((f x) ∷ map f xs)         ≡⟨ refl ⟩
      map g (map f (x ∷ xs))           ≡⟨ refl ⟩
      (map g ∙ map f) (x ∷ xs)
    ∎
```

Exercício
=========

- Prove o seguinte teorema.

```agda
++-map : ∀ {A B : Set}
           (f : A → B)
           (xs ys : List A) →
           map f (xs ++ ys) ≡ map f xs ++ map f ys
++-map f xs ys = {!!}
```

Exercício
=========

- Prove o seguinte teorema.

```agda
map-length : ∀ {A B : Set}
               (f : A → B)
               (xs : List A) → 
               length (map f xs) ≡ length xs
map-length f xs = {!!}
```

Reverse
=======

- Implementação de reverse

```agda
[_] : ∀ {A : Set} → A → List A
[ x ] = x ∷ []

reverse : ∀ {A : Set} → List A → List A
reverse [] = []
reverse (x ∷ xs) = reverse xs ++ [ x ]
```

Reverse
=======

- Relacionando concatenação e lista vazia.

```agda
++-[]-r : ∀ {A : Set}{xs : List A} → xs ++ [] ≡ xs
++-[]-r {_}{[]} = refl
++-[]-r {_}{x ∷ xs} = cong (_∷_ x) (++-[]-r {_}{xs})
```

Reverse
=======

- Teorema

```agda
++-reverse : ∀ {A : Set}(xs ys : List A) →
               reverse (xs ++ ys) ≡ reverse ys ++ reverse xs
++-reverse [] ys = sym ++-[]-r
++-reverse (x ∷ xs) ys
  = begin
      reverse (x ∷ xs ++ ys)              ≡⟨ refl ⟩
      reverse (xs ++ ys) ++ [ x ]         ≡⟨ cong (λ ys → ys ++ [ x ])
                                                  (++-reverse xs ys) ⟩
      (reverse ys ++ reverse xs) ++ [ x ] ≡⟨ ++-assoc (reverse ys)
                                                       (reverse xs)
                                                       [ x ] ⟩
      reverse ys ++ (reverse xs ++ [ x ]) ≡⟨ refl ⟩
      reverse ys ++ reverse (x ∷ xs)
    ∎
```

Reverse
=======

- Teorema

```agda
reverse-inv : ∀ {A : Set}(xs : List A) →
                reverse (reverse xs) ≡ xs
reverse-inv [] = refl
reverse-inv (x ∷ xs)
  = begin
      reverse (reverse (x ∷ xs)) ≡⟨ refl ⟩
      reverse (reverse xs ++ [ x ]) ≡⟨ ++-reverse (reverse xs) [ x ] ⟩
      reverse [ x ] ++ reverse (reverse xs) ≡⟨ cong (_∷_ x)
                                                    (reverse-inv xs) ⟩
      reverse [ x ] ++ xs ≡⟨ refl ⟩
      [ x ] ++ xs ≡⟨ refl ⟩
      x ∷ xs
    ∎
```

Exercício
=========

- Prove o seguinte teorema

```agda
reverse-map : ∀ {A B : Set}
                (f : A → B)
                (xs : List A) →
                reverse (map f xs) ≡ map f (reverse xs)
reverse-map f xs = {!!}
```

Fold-Map Fusion
===============

- Função foldr

```agda
foldr : ∀ {A B : Set} → (A → B → B) → B → List A → B
foldr f v [] = v
foldr f v (x ∷ xs) = f x (foldr f v xs)
```

Fold-Map Fusion
===============

- Teorema

```agda
fold-map-fusion : ∀ {A B C : Set}
                    (f : A → B)
                    (g : B → C → C)
                    (v : C)
                    (xs : List A) →
 (foldr g v ∙ map f) xs ≡ (foldr (g ∙ f) v xs)
```

Fold-Map Fusion
===============

- Demonstração.

```agda
fold-map-fusion f g v [] = refl
fold-map-fusion f g v (x ∷ xs)
   = begin
       (foldr g v ∙ map f) (x ∷ xs)       ≡⟨ refl ⟩
       foldr g v (map f (x ∷ xs))         ≡⟨ refl ⟩
       foldr g v (f x ∷ map f xs)         ≡⟨ refl ⟩
       g (f x) (foldr g v (map f xs))     ≡⟨ refl ⟩
       (g ∙ f) x ((foldr g v ∙ map f) xs) ≡⟨ cong (λ y → g (f x) y)
                                                  (fold-map-fusion f g v xs) ⟩
       (g ∙ f) x (foldr (g ∙ f) v xs)     ≡⟨ refl ⟩
       foldr (g ∙ f) v (x ∷ xs)
     ∎
```

Pertinência
===========

- Predicado para pertinência.

```agda
data _∈_ {A : Set} : A → List A → Set where
  here  : ∀ {x xs} → x ∈ (x ∷ xs)
  there : ∀ {x y ys} → x ∈ ys → x ∈ (y ∷ ys)
```

Pertinência
===========

- Relacionando a pertinência e concatenação.

```agda
++-∈-l : ∀ {A : Set}{xs ys : List A}{x : A} →
           x ∈ (xs ++ ys) → x ∈ xs ⊎ x ∈ ys
++-∈-l {xs = []} pr = inr pr
++-∈-l {xs = x ∷ xs} here = inl here
++-∈-l {xs = x ∷ xs} (there pr) with ++-∈-l pr
...| inl pr1 = inl (there pr1)
...| inr pr2 = inr pr2
```

Pertinência
===========

- Relacionando a pertinência e concatenação.

```agda
++-∈-r-1 : ∀ {A : Set}{xs : List A}{x : A} ys →
             x ∈ xs → x ∈ (xs ++ ys)
++-∈-r-1 ys here = here
++-∈-r-1 ys (there p)
  = there (++-∈-r-1 ys p)
```

Pertinência
===========

- Exercício: Prove a seguinte propriedade

```agda
++-∈-r-2 : ∀ {A : Set}{ys : List A}{x : A} xs →
             x ∈ ys → x ∈ (xs ++ ys)
++-∈-r-2 = {!!}
```
