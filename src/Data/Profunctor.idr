module Data.Profunctor

import Data.Morphisms

public export
interface Profunctor (0 f : Type -> Type -> Type) where
  dimap : (a -> a') -> (b -> b') -> f a' b -> f a b'
  lmap : (a -> a') -> f a' b -> f a b
  rmap : (b -> b') -> f a b -> f a b'

  dimap pre post = rmap post . lmap pre
  lmap pre = dimap pre id
  rmap post = dimap id post
