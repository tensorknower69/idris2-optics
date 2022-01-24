module Control.Optics.Either

import public Control.Optics.Prism

export
pleft : Prism (Either a c) (Either b c) a b
pleft = prism Left (either Right (Left . Right))
