module Data.Functor.Tagged

import Data.Profunctor
import Data.Profunctor.Choice

public export
data Tagged : (a : Type) -> (b : Type) -> Type where
  MkTagged : b -> Tagged a b

export
retag : Tagged s b -> Tagged t b
retag (MkTagged b) = MkTagged b

export
untag : Tagged s b -> b
untag (MkTagged b) = b

export
Semigroup b => Semigroup (Tagged a b) where
  MkTagged x <+> MkTagged x' = MkTagged (x <+> x')

export
Monoid b => Monoid (Tagged a b) where
  neutral = MkTagged neutral

export
Functor (Tagged a) where
  map f (MkTagged x) = MkTagged (f x)

export
Applicative (Tagged a) where
  pure = MkTagged
  (MkTagged f) <*> (MkTagged x) = MkTagged (f x)

export
Monad (Tagged a) where
  MkTagged m >>= k = k m

export
Bifunctor Tagged where
  bimap f g (MkTagged b) = MkTagged (g b)

export
Profunctor Tagged where
  lmap _ = retag
  rmap = map

export
Choice Tagged where
  left' (MkTagged b) = MkTagged (Left b)
  right' (MkTagged b) = MkTagged (Right b)
