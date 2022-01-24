module Data.Profunctor.Morphism

import public Data.Morphisms
import Data.Profunctor
import Data.Profunctor.Choice

public export
by : (Morphism a b -> Morphism c d) -> (a -> b) -> (c -> d)
by f = applyMor . f . Mor

export
Profunctor Morphism where
  lmap pre  = Mor . (. pre) . applyMor
  rmap post = Mor . (post .) . applyMor

export
Choice Morphism where
  right' = Mor . map . applyMor
