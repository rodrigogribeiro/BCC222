---
author: Programação Funcional
title: Introdução às Mônadas
date: Prof. Rodrigo Ribeiro
---

Setup
=====

> module SlidesKmeans where

Motivação
=========

- Recomendação em sistemas web

- Direcionamento de promoções, propaganda, etc.

Motivação
=========

- Implementação do algoritmo K-Means para
clusterização de dados.

- Usaremos o K-Means como um exemplo para
a necessidade de manipular estado, uma
aplicação do conceito de mônadas.

K-Means
=======

- Um dos algoritmos mais simples para
clusterização de dados.

- Dados representados como um conjunto
de "pontos" no espaço.


Clustering
==========

![Clustering](clusters.png){#id .class width=530 height=420px}

K-Means
=======

![Algoritmo K-Means](kmeans.png){#id .class width=220 height=550px}


K-Means
=======

- Mas como representar padrões de compra como pontos?

- Detalhes... implementação como uma classe de tipos.

Vector
======

```haskell
{-# LANGUAGE FlexibleInstances #-}

type Point = (Double,Double)

class Vector v where
  distance :: v -> v -> Double

instance Vector Point where
  distance (x1,y1) (x2,y2)
    = sqrt $ (x2 - x1) ^ 2 + (y2 - y1) ^ 2
`````

Conversão
=========

```haskell
{-# LANGUAGE MultiParamTypeClasses #-}

class Vector v => Vectorizable e v where
  toVector :: e -> v

instance Vectorizable Point Point where
  toVector = id
```

K-Means
=======

- Tradicionalmente, o número de clusters
é um parâmetro do algoritmo.

- Nossa implementação seguirá outra
estratégia: o algoritmo será parametrizado
por uma função para geração inicial dos clusters.

K-Means
=======

```haskell
kMeans :: Vectorizable e v    =>
          (Int -> [e] -> [v]) -> -- initialization function
          Int                 -> -- number of centroids
          [e]                 -> -- data
          Double              -> -- limit
          [v]                    -- final centroids
```

Cluster Phase
=============

- Recebe os centroids atuais e elementos.

- Atribui elementos mais próximos de cada centroid
para formar um novo cluster.


Cluster Phase
=============

```haskell
clusterPhase :: Vectorizable e v => [v] -> [e] -> M.Map v [e]
clusterPhase centroids points
  = let
      initialMap = M.fromList $ zip centroids (repeat [])
    in foldr step initialMap points
  where
    step p m = let chosen = minimumBy (cmp p)
                                      centroids
                in M.adjust (p :) chosen m
    cmp p x y = compare (distance x $ toVector p)
                        (distance y $ toVector p)
```

Centroids
=========

- Calcular novos centroids a partir dos clusters atualizados.

- Para isso, devemos modificar a classe Vector para incluir
uma função que calcula um novo centroid a partir de um conjunto
de pontos.

Vector, Again
=============

```haskell
class Ord v => Vector v where
   distance :: v -> v -> Double
   centroid :: [v] -> v
```

Vector, Again
=============

```haskell
instance Vector Point where
  distance (x1,y1) (x2,y2)
    = sqrt $ (x2 - x1) ^ 2 + (y2 - y1) ^ 2

  centroid xs
    = let
        (a,b) .+. (c,d) = (a + c, b + d)
        (u,v) = foldr (.+.) (0,0) xs
        n = fromIntegral $ length xs
      in (u / n, v / n)
```

Centroids
=========

```haskell
newCentroidPhase :: ( Vector v
                    , Vectorizable e v) =>
                    M.Map v [e]         ->
                    [(v,v)]
newCentroidPhase = M.toList . fmap (centroid . map toVector)
```

Stop!
====

- Condição de parada do K-Means.

```haskell
shouldStop :: Vector v => [(v,v)] -> Double -> Bool
shouldStop centroids limit
  = foldr step 0.0 centroids < limit
  where
   step (x,y) ac = ac + distance x y
```

K-Means
=======

```haskell
kMeans' :: Vectorizable e v =>
           [v]              ->
           [e]              ->
           Double           ->
           [v]
kMeans' centroids points limit
  = let
      clusters = clusterPhase centroids points
      oldCentroids = newCentroidPhase clusters
      newCentroids = snd <$> oldCentroids
    in if shouldStop oldCentroids limit
       then newCentroids
       else kMeans' newCentroids points limit
```

K-Means
=======

```haskell
kMeans :: Vectorizable e v    =>
          (Int -> [e] -> [v]) -> -- initialization function
          Int                 -> -- number of centroids
          [e]                 -> -- data
          Double              -> -- limit
          [v]                    -- final centroids
kMeans i k points
  = kMeans' (i k points) points
```

Exercícios
==========

- Implemente uma versão do algoritmo K-Means que retorne
os clusters construídos ao invés dos centroids.

- Modifique o algoritmo para que ele retorne, como
parte do resultado, o número de chamadas recursivas
realizadas durante a execução.

KMeans
======

- Em livros, a implementação do KMeans atualiza
os valores de centroids e erro atual durante a
execução.

- É possível fazer isso em Haskell?

Estado
======

```haskell
data KMeansState e v
  = KMeansState {
      _centroids :: [v]     -- current centroids
    , _points    :: [e]     -- points considered
    , _err       :: Double  -- current error value
    , _limit     :: Double  -- mininum accepted error
    , _steps     :: Int     -- number of steps
    }
```
Modificando
===========

```haskell
kMeans' :: Vectorizable e v =>
           KMeansState e v  ->
           KMeansState e v
kMeans' state
  = let
      genCentroids = centroid . map toVector
      clusters = clusterPhase state
      new = snd <$> (M.toList $ genCentroids <$> clusters)
````

Continuando...
==============

```haskell
      -- state update 1
      state1 = state { _centroids = new }
      n_err = sum $ zipWith distance (_centroids state)
                                     (_centroids state1)
      -- state update 2
      state2 = state1 { _err = n_err }
      -- state update 3
      state3 = state2 { _steps = (_steps state2) + 1 }
    in if (_err state3) < (_limit state3)
       then state3
       else kMeans' state3
```

KMeans
======

- Suposição: Dados completamente fornecidos.

- Realidade: Muitas vezes, dados devem ser pré-processados
para remover inconsistências.

- Dados em BDs podem conter informações nulas.

KMeans
======

- Informações possivelmente nulas são representadas pelo
tipo Maybe.

- Problema: Tipo Maybe pode gerar casamentos de padrão
que poluem o código.

Exemplo
=======

```haskell
averagePurchase :: Integer -> -- Id do cliente
                   Double     -- média de preços
averagePurchase cliId
  = let ps = purchasesByClientId cliId
      in sum $ catMaybes $ map purchaseValue ps
```

Exemplo
=======

```haskell
purchaseValue :: Integer -> Maybe Double
purchaseValue purchaseId
  = case numberOfItemsByPurchaseId purchaseId of
       Nothing -> Nothing
       Just n  ->
         case productIdByPurchaseId purchaseId of
            Nothing -> Nothing
            Just prId ->
              case priceByProdutId prId of
                Nothing -> Nothing
                Just price -> Just (fromInteger n) * price

```

Problemas
=========

- Casamentos de padrão aninhados são de difícil compreensão
e manutenção.

- Estrutura do casamento de padrão:
    - Se o valor é Nothing, este valor é retornado
    - Se o valor é Just v, use v para continuar o cálculo.

Refatorando
===========

- Estrutura de casamento como uma função:

```haskell
thenDo :: Maybe a -> (a -> Maybe b) -> Maybe b
thenDo Nothing  _ = Nothing
thenDo (Just v) f = f v
```

Refatorando
===========

```haskell
purchaseValue :: Integer -> Maybe Double
purchaseValue purchaseId
  = numberOfItemsByPurchaseId purchaseId `thenDo` (\ n ->
    productIdByPurchaseId purchaseId `thenDo` (\prId ->
    priceByProductId prId `thenDo` (\ price ->
    Just $ fromInteger n * price)))
```

Resultado
=========

- Código mais legível: casamento de padrão fatorado

- Porém, porquê usar a função thenDo? Não seria
possível de outra forma?


thenDo
======

- Note que o tipo de thenDo é um pouco parecido
com o de fmap para o tipo Maybe:

```haskell
thenDo    :: Maybe a -> (a -> Maybe b) -> Maybe b
flip fmap :: Maybe a -> (a ->       b) -> Maybe b
                              ^^^^^^
```

- Pergunta: Qual a relação entre essas funções?

thenDo e fmap
=============

- Observe que o tipo de fmap deveria ser:

```haskell
fmap   :: (a -> Maybe b) -> Maybe a -> Maybe (Maybe b)
fmap f v = v `thenDo`(\ x -> Just (f x))
````

thenDo e fmap
=============

- Logo, a abstração fornecida por thenDo é mais
expressiva que fmap.

- Mas, será que thenDo aplica-se somente ao
tipo Maybe?

- Vamos tentar usá-la na manipulação de estado
do algoritmo KMeans.

Estado
======

```haskell
type State s a = s -> (a, s)
```

- Parâmetro s: tipo do estado.
- Parâmetro a: valor retornado após
uma possível atualização de estado.

Estado
======

- Manipulação de estado envolve as
seguintes operações:

```haskell
remain :: a -> State s a
remain v = \ s -> (v , s)

access :: (s -> a) -> State s a
access f = \ s -> (f s, s)

modify :: (s -> s) -> State s ()
modify f = \ s -> ((), f s)
```

Estado
======

- ThenDo para estados.

```haskell
thenDo :: State s a -> (a -> State s b) -> State s b
thenDo f g s
  = let (result,newState) = f s
      in g result newState
```

KMeans
======

```haskell
kMeans' :: Vectorizable e v =>
           [e]              ->
           State (KMeansState v) [v]
kMeans' points
  = access centroids                      `thenDo` (\ ps   ->
    remain (clusterPhase ps points)       `thenDo` (\ asgn ->
    remain (newCentroids asgn)            `thenDo` (\ ns   ->
    modify (\s -> s{centroids = ns})      `thenDo` (\ _    ->
    modify (\s -> s{steps = steps s + 1}) `thenDo` (\ _    ->
    access limit                          `thenDo` (\ lim  ->
    remain (sum $ zipWith distance ps ns) `thenDo` (\ err  ->
    if err < lim then remain ns else kMeans' points)))))))
```

ThenDo
======

- O padrão por trás da implementação de ThenDo para
Maybe e State é conhecido como Mônada.

- Mônadas são usadas em Haskell para separação de
código que possui efeitos colaterais de código
puro.

Monad
=====

- Monadas são uma classe de tipos

```haskell
class Functor f => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

class Applicative m => Monad m  where
  return :: a -> m a
  (>>=)  :: m a -> (a -> m b) -> m b
`````

Monad para Maybe
================

```haskell
instance Functor Maybe where
  fmap f Nothing = Nothing
  fmap f (Just x) = Just (f x)

instance Applicative Maybe where
  pure = Just
  Nothing  <*> _        = Nothing
  _        <*> Nothing  = Nothing
  (Just f) <*> (Just x) = Just (f x)

instance Monad Maybe where
  return = Just
  Nothing  >>= _ = Nothing
  (Just v) >>= f = f v
```

Monad para State
================

```haskell
instance Functor State where
  fmap f m = \s -> let (res,s') = m s in (f res, s')

instance Applicative State where
  mf <*> mx = \ s -> let
                       (f,s') = mf s
                       (x,s'') = mx s'
                     in (f x, s'')

instance Monad State where
  ma >>= f = \ s -> let
                      (x,s') = ma s
                    in f x s'
```

Do Notation
===========

- Notação para escrita de código envolvendo
mônadas.

```haskell
... = do
       x <- f1
       y <- f2
       g x y
```
é equivalente a

```haskell
f1 >>= \ x -> f2 >>= \y -> g x y
```

Monad State
===========

- Mônada de estado está disponível no módulo
Control.Monad.State.

- Funcionalidades:

```haskell
put :: s -> State () s
get :: State s s
gets :: (s -> a) -> State s a
modify :: (s -> s) -> State s ()
```

KMeans - Monad
==============

```haskell
kMeans' :: Vectorizable e v =>
           [e]              ->
           State (KMeansState v) [v]
kMeans' points
  = gets centroids                           >>= (\ prevs ->
    pure (clusterPhase prevs points)         >>= (\ asgn ->
    pure (newCentroids asgn)                 >>= (\ news ->
    modify (\s -> s{centroids = news})       >>= (\ _ ->
    modify (\s -> s{steps = steps s + 1})    >>= (\ _ ->
    gets limit                               >>= (\ lim ->
    pure (sum $ zipWith distance prevs news) >>= (\ err ->
    if err < lim then pure news else kMeans' points)))))))
```

KMeans - Do
===========

```haskell
kMeans' :: Vectorizable e v =>
           [e]              ->
           State (KMeansState v) [v]
kMeans' points = do
      prevs <- gets centroids
      let assgn = clusterPhase prevs points
          news  = newCentroids assgn
      modify (\s -> s{centroids = news})
      modify (\s -> s{steps = steps s + 1})
      lim <- gets limit
      let
        err = sum $ zipWith distance prevs news
      if err < lim then return news else kMeans' points
```
