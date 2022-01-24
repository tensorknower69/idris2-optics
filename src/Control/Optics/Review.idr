module Control.Optics.Review

import Control.Monad.Identity
import Data.Profunctor.Morphism
import public Control.Optics.Getter

export
(#) : AReview t b -> b -> t
(#) p = runIdentity . untag . p . MkTagged . Id

export
re : AReview t b -> Getter b t
re p = to (\x => p # x)
