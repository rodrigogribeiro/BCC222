---
title: Estudo de caso - Lenses
author: Programação Funcional
date: Prof. Rodrigo Ribeiro
---

Setup
=====

> module Aula12 where


Motivação
=========

- Registros são uma forma útil
de estruturar informação.

- Modificar registros é algo tedioso.

Registros
=========

- Exemplo de registro:

```haskell
data Point
  = Point {
      x :: Int
    , y :: Int
    }
```

Registros
=========

- Campos definem funções de projeção:

```haskell
x :: Point -> Int
y :: Point -> Int
```

Registros
=========

- Atualização de registros

```haskell
setX :: Int -> Point -> Point
setX x' p = p {x = x'}
```

- Sintaxe permite modificar apenas
um certo campo.

- Ok, se a estrutura for pequena...

Motivação
=========

```haskell
data Address
  = Address {
      _street :: String
    , _city :: String
    , _cep  :: String
    } deriving Show

data Person
  = Person {
      _name :: String
    , _age :: Int
    , _address :: Address
    } deriving Show
```

Motivação
=========

- Como alterar a rua do endereço
de uma pessoa?

```haskell
setStreet :: String -> Person -> Person
setStreet st p
   = p{_address
        = (_address p){_street = s}}
```

Problema
========

- Função `setStreet` faz a atualização do
endereço

```haskell
(_address p){_street = s}
```

e depois da pessoa

```haskell
p {_address = ...}
```

Pergunta
========

- Em linguagens OO isso seria resolvido
facilmente usando funções set/get
    - Brainless code gerado por IDEs

- Haskell não possui suporte a esse
tipo de código?

Solução
========

- Lenses: get/set technology para
Haskell

- Construído usando conceitos de
applicative functors


Implementação 1
===============

- Lens é um registro formado por
uma função view e set

```haskell
data Lens s a
  = Lens {
      _view :: s -> a      -- get
    , _set  :: a -> s -> s -- set
    }
````

Lens
====

- Registro contendo um par de funções.

- `_view :: s -> a`: obter valor de tipo `a` em
uma estrutura de tipo `s`

- `_set :: a -> s -> s`: atualizar valor de tipo `a`
na estrutura de tipo `s`.

Exemplo
=======

- Lens para o campo `_name` do tipo `Person`.

```haskell
name :: Lens Person String
name
  = Lens _name setName
    where
      setName s p = p{_name = s}
```

Exemplo
=======

- Lens para o campo `_address` do tipo `Person`.

```haskell
address :: Lens Person Address
address = Lens _address setAddress
  where
    setAddress a p = p{_address = a}
```

Exercício
=========

1. Implemente o lens para o campo `_age` do
tipo `Person`.

2. Implemente os lenses para os campos do
tipo `Address`.

Lenses
======

- Até aí, apenas um tipo para representar get/set.

- Qual a vantagem de se usar lenses?
    - Lenses são composicionais!

Composição
==========

```haskell
(@@) :: Lens s s1 -> Lens s1 a -> Lens s a
(Lens v1 s1) @@ (Lens v2 s2)
  = Lens (v2 . v1)
         (\ a s -> s1 (s2 a (v1 s)) s)
```

Composição
==========

![](lens-composition.png){ width=650px }

Exemplos
========

```haskell
streetOf :: Person -> String
streetOf = _view (address @@ street)

setStreet :: String -> Person -> Person
setStreet s = _set (address @@ street)
```

Exemplo
=======

- Atualizar a rua para "Rua 6".

```haskell
pex :: Person
pex = Person "João" 30 addex

addex :: Address
addex = Address "Rua A" "São Pedro" "123"
```

Exemplo
=======

```haskell
*Prelude> setStreet "Rua 6" pex
Person
  {_name = "João"
  , _age = 30
  , _address
    = Address
      {_street = "Rua 6"
      , _city = "São Pedro"
      , _cep = "123"}}
```

Só registros?
=============

- Não! Lenses são um padrão que permite
  modificar estruturas quaisquer de forma
  composicional.

Exemplo
=======

- Lens para listas

```haskell
setIdx :: Int -> a -> [a] -> [a]
setIdx _ _ [] = error "index too large"
setIdx i v (x : xs)
  | i < 0    = error "negative index"
  | otherwise = if i == 0
                then v : xs
                else x : setIdx (i - 1) v xs

ix :: Int -> Lens [a] a
ix i = Lens (!! i) (setIdx i)
```

Exemplo
=======

- Incrementar um elemento de uma lista?
   - Vamos generalizar para aplicar uma função
     sobre elementos da lista.

```haskell
atPos :: (a -> a) -> Int -> [a] -> [a]
atPos f i xs
  = let
      v  = _view (ix i) xs
      v' = f v
    in _set (ix i) v' xs
```

Problema
========

- Veja que a implementação anterior
  percorre a lista duas vezes.
    - Uma vez para encontrar o elemento.
    - Outra vez para atualizar a lista original.
- Ineficiente...

Solução?
=======

- Adicionar uma função para modificar um
  campo.

```haskell
data LensR s a
  = LensR {
      _viewR   :: s -> a
    , _setR    :: a -> s -> s
    , _modifyR :: (a -> a) -> s -> s
    }
```

Solução?
========

- O tipo de `_modifyR` define funções
  de modificação que nunca "falham".

```haskell
_modifyR :: (a -> a) -> s -> s
```

- E se quisermos dividir um campo
  numérico por um valor?

Solução?
========

- Podemos adicionar outro campo...

```haskell
data LensR s a
  = LensR {
      _viewR   :: s -> a
    , _setR    :: a -> s -> s
    , _modifyR :: (a -> a) -> s -> s
    , _modifyM :: (a -> Maybe a) -> s -> Maybe s
    }
```

Solução?
========

- E se precisarmos aplicar uma função
  sobre dados recebidos via rede?
    - Mais um campo sobre a mônada de I/O...

```haskell
data LensR s a
  = LensR {
      _viewR   :: s -> a
    , _setR    :: a -> s -> s
    , _modifyR :: (a -> a) -> s -> s
    , _modifyM :: (a -> Maybe a) -> s -> Maybe s
    , _modifyIO :: (a -> IO a) -> s -> IO s
    }
```

Solução?
========

- Essa repetição excessiva de campos
  não é um bom sinal.

- Vamos observar os tipos dos campos
  incluídos:

```haskell
_modifyR  :: (a -> a) -> s -> s
_modifyM  :: (a -> Maybe a) -> s -> Maybe s
_modifyIO :: (a -> IO a) -> s -> IO s
````

Padrão?
=======

- Vamos nos concentrar nos dois últimos:

```haskell
_modifyM  :: (a -> Maybe a) -> s -> Maybe s
_modifyIO :: (a -> IO    a) -> s -> IO    s
````

- Padrão? Sim, tanto Maybe quanto IO são
  functores!

Padrão?
======

- Generalizando modify:

```haskell
modifyF :: Functor f => (a -> f a) -> s -> f s
```

- Porém, isso é tão geral que podemos representar
  um Lens usando apenas essa função.

Implementação 2
===============

- Uma nova definição de Lens.

```haskell
{-# LANGUAGE RankNTypes #-}

type Lens s a = Functor f => (a -> f a) -> s -> f s
```

- Como assim???
    - Veremos que essa definição de Lens é equivalente
      à definição da primeira implementação.

Definindo `set`
==============

- Como definir a função `set`?

```haskell
type Lens s a = Functor f => (a -> f a) -> s -> f s

set :: Lens s a -> a -> s -> s
set ln a s = _
```

- Como obter um valor `s` se o retorno de `ln` é
  `(f s)`?

Definindo `set`
===============

- Escolhendo um `f`:
   - Deve ser possível obter `s` a partir de `f s`.
   - Restrição: deve ser uma instância de `Functor`.

Definindo `set`
==============

- Solução: Identity Functor

```haskell
newtype Identity a = Identity a


runIdentity :: Identity a -> a
runIdentity (Identity v) = v


instance Functor Identity where
  fmap f (Identity v)
    = Identity (f v)
```

Definindo `set`
==============

```haskell
set :: Lens s a -> a -> s -> s
set ln v s
    = runIdentity (ln set_fld s)
  where
    set_fld _ = Identity v
```

Definindo `over`
==============

- Aplicando uma função
    - Desta vez, a alteração é feita localmente.
    - Eficiente, não há "get depois set"

```haskell
over :: Lens s a -> (a -> a) -> s -> s
over ln f s
  = runIdentity (ln (Identity . f) s)
```

Definindo `view`
================

- Problema: Como obter `a` a partir de `(f s)`?
   - Novamente, vamos escolher um `f` apropriado.


```haskell
type Lens s a = Functor f => (a -> f a) -> s -> f s

view :: Lens s a -> s -> a
view ln s = _
```

Constant Functor
================

```haskell
newtype Const a b = Const a

getConst :: Const a b -> a
getConst (Const v) = v

instance Functor (Const a) where
  fmap _ (Const v) = Const v
```

Definindo `view`
===============

```haskell
view :: Lens s a -> s -> a
view ln s
  = getConst (ln Const s)
```

Equivalência
============

- Usando as definições anteriores,
  podemos mostrar a equivalência
  entre as implementações.

```haskell
lens2lensD :: Lens s a -> D.Lens s a
lens2lensD ln
  = D.Lens (view ln)
           (set ln)
```

Equivalência
============

- Definição auxiliar: Criar um lens a partir
  de funções get/set

```haskell
lens :: (s -> a) -> (a -> s -> s) -> Lens s a
lens vw st trans s
  = flip st s <$> trans (vw s)
```

Equivalência
============

- Usando lens, a segunda parte da equivalência é
  imediata.

```haskell
lensD2lens :: D.Lens s a -> Lens s a
lensD2lens (D.Lens vw st)
  = lens vw st
```

Exemplo
=======

- Lens para o campo `_name` do tipo `Person`

```haskell
name :: Lens Person String
name
  = lens _name setName
    where
      setName n (Person _ ag ad)
        = Person n ag ad
```

Exercício
==========

- Crie lenses para os demais campos
  dos tipos `Person` e `Address`.

Exemplo
=======

```haskell
data Employee
  = Employee {
       _name   :: String
    ,  _salary :: Int
    }

name :: Lens Employee String
name fs (Employee n s)
  = (\ n' -> Employee n' s) <$> (fs n)

joe :: Employee
joe = Employee "Joe" 100
```

View
====

```haskell
view name (Employee "Joe" 100) ==>
-- substituindo view
getConst (name Const (Employee "Joe" 100)) ==>
-- substituindo name
getConst (fmap (\ n' -> Employee n' 100) (Const "Joe")) ==>
-- fmap f (Const v) = v
getConst (Const "Joe") ==>
-- getConst (Const v) = v
"Joe"
```

Composição
==========

- Composição de lenses tem o tipo:

```haskell
Lens s s1 -> Lens s1 a -> Lens s a
   ln1         ln2
```

- que expandido resulta em

```haskell
ln1 :: (s1 -> f s1) -> s  -> f s
ln2 :: (a  -> f a ) -> s1 -> f s1
```

Composição
==========

- Logo, o tipo de `ln1 . ln2` é:

```haskell
(a -> f a) -> s -> f s
```

que é exatamente `Lens s a`.

- Composição de lenses é composição
  de funções.

Operadores
==========

- Notação para view and set.

```haskell
(^.) :: s -> Lens s a -> a
s ^. ln = view ln s

(~.) :: Lens s a -> a -> s -> s
(~.) = set
```

Exemplos
========

- Retornando aos dados de exemplo

```haskell
pex :: Person
pex = Person "João" 30 addex

addex :: Address
addex = Address "Rua A" "São Pedro" "123"
```

Exemplos
========

```haskell
Person> pex ^. (address . street)
"Rua A"
Person> (pex & name ~. "Carlos") ^. name
"Carlos"

Person> let street' = address . street
Person> (pex & street' ~. "Baker Street") ^. street'
"Baker Street"
```

Aplicações
==========

- "Campos virtuais"
    - Campo Farenheint pode ser atualizado,
      consultado sem existir!

```haskell
data Temp
  = Temp {
      _celsius :: Float
    } deriving Show

celsius :: Lens Temp Float
celsius = ...

farenheint :: Lens Temp Float
farenheint f (Temp c)
  = (\ fa -> Temp (ftc fa)) <$> (f (ctf c))

-- ftc, ctf :: Float -> Float
```

Aplicações
==========

- Manter invariantes.
  - Modificar valores de horas.

```haskell
data Time
  = Time {
      _hours :: Int
    , _mins :: Int
    } deriving Show

now :: Time
now = Time 3 58
```

Aplicações
==========

- Manter invariantes

```haskell
mins :: Lens Time Int
mins f (Time h m)
  = wrap <$> (f m)
  where
    wrap :: Int -> Time
    wrap m'
      | m' >= 60  = Time (h + 1) (m' - 60)
      | m' < 0    = Time (h - 1) (m' + 60)
      | otherwise = Time h m'
```

Aplicações
==========

- Manter invariantes

```haskell
Prelude> over (+ 4) now
Time 4 2
```

Problema
========

- Como converter a rua e cidade
  do endereço de uma pessoa para
  letras minúsculas?

Problema
========

- Usando nossa solução atual,
  temos que realizar duas
  atualizações.

- Como fazer em um único passo?

Solução
=======

- Applicative Lenses!
    - Permite "focar" em múltiplos valores de uma
      estrutura.
- Definição:

```haskell
type ALens s a
  = forall f. Applicative f => (a -> f a) -> s -> f s
```

Set
===

- A definição de set para `Lens` funciona para `ALens`!
   - Identity como instância de Applicative.

```haskell
instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity v)
    = Identity (f v)

set :: ALens s a -> a -> s -> s
set ln a s
  = runIdentity (ln set_fld s)
  where
    set_fld _ = Identity a
```

view?
=====

- A implementação de view é a mesma?
    - Não! Como combinar vários valores
    - Lembre-se o functor de view é Const!
- Solução: Monoids!

```haskell
class Monoid a where
  mempty  :: a
  mappend :: a -> a -> a
```

view?
=====

- Para criar a instância de Applicative para
  `Const v a` devemos exigir que `a` seja
  instância de `Monoid`

```haskell
instance Monoid a => Applicative (Const a) where
  pure _ = Const mempty
  (Const f) <*> (Const v) = Const (f `mappend` v)
```

view?
=====

- Implementação de `view`

```haskell
view :: Monoid a => ALens s a -> s -> a
view ln s
  = getConst (ln Const s)
```

Exemplo
=======

- Applicative lenses para endereços

```haskell
address :: ALens Person Address
address f (Person n a ad)
  = (\ ad' -> Person n a ad') <$> f ad

streetCity :: ALens Address String
streetCity f (Address st ct cp)
  = (\ st' ct' -> Address st' ct' cp) <$>
        f st <*> f ct
```

Exemplo
=======

- Modificando endereço para letras minúsculas.

```haskell
Prelude> over (address . streetCity) (map toLower) pex
Person {
  _name = "João"
, _age = 30
, _address
   = Address
      { _street = "rua a",
        _city = "nova iorque",
        _cep = "123"}}
```

Finalizando
===========

- Lenses é um tópico vastíssimo!
   - Implementações apresentadas são super
     "simples". 
   - Várias bibliotecas Haskell e em outras linguagens:
     Scala, Elm, Kotlin, Typescript, Rust, etc...

Finalizando
===========

- Composição: essência do desenvolvimento de software.
   - Pequenas abstrações e formas de combiná-las.

- Functores aplicativos são uma forma elegante de
  implementar software composicional.
