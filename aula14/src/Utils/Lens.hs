{-# LANGUAGE RankNTypes #-}

module Utils.Lens ( Lens
                  , set
                  , over
                  , view
                  , lens
                  , (&)
                  , (^.)
                  , (~.)) where

import Data.Functor.Identity
import Data.Functor.Const
import Data.Function

type Lens s a
  = forall f. Functor f =>
      (a -> f a) -> s -> f s


set :: Lens s a -> a -> s -> s
set ln a s
  = runIdentity (ln set_fld s)
  where
    set_fld _ = Identity a


over :: Lens s a -> (a -> a) -> s -> s
over ln f s
  = runIdentity (ln (Identity . f) s)


view :: Lens s a -> s -> a
view ln s
  = getConst (ln Const s)


lens :: (s -> a) -> (a -> s -> s) -> Lens s a
lens vw st trans s
  = flip st s <$> trans (vw s)

-- a simple operator for view


infixl 8 ^.

(^.) :: s -> Lens s a -> a
s ^. ln = view ln s

-- operator for set

(~.) :: Lens s a -> a -> s -> s
(~.) = set
