module Control.Optics.Prism

import public Control.Optics.Types

export
prism : (b -> t) -> (s -> Either t a) -> Prism s t a b
prism inj prj = dimap prj (either pure (map inj)) . right'
