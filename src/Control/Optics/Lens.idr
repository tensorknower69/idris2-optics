module Control.Optics.Lens

import public Control.Optics.Types

export
lens : (prj : s -> a) -> (inj : s -> b -> t) -> Lens s t a b
lens prj inj (Mor f) = Mor (\s => inj s <$> f (prj s))
