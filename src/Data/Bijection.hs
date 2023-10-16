module Data.Bijection where

import Control.Monad.Identity

-- Biderectional arrows `f` from `a` to `b`
-- in a functorial context.
class (Functor f) => Bijection m f a b where
  fwd :: m -> f (a -> b)
  bwd :: m -> f (b -> a)

  (->>) :: a -> m -> f b
  a ->> m = ($ a) <$> fwd m

  (<<-) :: b -> m -> f a
  b <<- m = ($ b) <$> bwd m

instance Bijection (a -> b, b -> a) Identity a b where
  fwd = return . fst
  bwd = return . snd

instance (Functor f) => Bijection (f (a -> b, b -> a)) f a b where
  fwd = fmap fst
  bwd = fmap snd
