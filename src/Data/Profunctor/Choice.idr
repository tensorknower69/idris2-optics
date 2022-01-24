module Data.Profunctor.Choice

import Data.Profunctor

public export
interface Profunctor p => Choice p where
  left' : p a b -> p (Either a c) (Either b c)
  right' : p a b -> p (Either c a) (Either c b)

  right' = dimap (either Right Left) (either Right Left) . left'
  left' = dimap (either Right Left) (either Right Left) . right'
