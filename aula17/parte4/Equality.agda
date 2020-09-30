module Equality where

infix 4 _≡_

data _≡_ {A : Set}(x : A) : A → Set where
  refl : x ≡ x

{-# BUILTIN EQUALITY _≡_ #-}

trans : ∀ {A : Set}
          {x y z : A} → x ≡ y
                      → y ≡ z
                      → x ≡ z
trans refl refl = refl

sym : ∀ {A : Set}{x y : A} → x ≡ y → y ≡ x
sym refl = refl

cong : ∀ {A B : Set}{x y : A} →
         (f : A → B)          →
         x ≡ y                →
         f x ≡ f y
cong f refl = refl


module ≡-Reasoning {A : Set} where

  infix  1 begin_
  infixr 2 _≡⟨⟩_ _≡⟨_⟩_
  infix  3 _∎

  begin_ : ∀ {x y : A}
    → x ≡ y
      -----
    → x ≡ y
  begin x≡y  =  x≡y

  _≡⟨⟩_ : ∀ (x : A) {y : A}
    → x ≡ y
      -----
    → x ≡ y
  x ≡⟨⟩ x≡y  =  x≡y

  _≡⟨_⟩_ : ∀ (x : A) {y z : A}
    → x ≡ y
    → y ≡ z
      -----
    → x ≡ z
  x ≡⟨ x≡y ⟩ y≡z  =  trans x≡y y≡z

  _∎ : ∀ (x : A)
      -----
    → x ≡ x
  x ∎  =  refl

open ≡-Reasoning public
