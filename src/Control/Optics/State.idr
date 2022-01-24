module Control.Optics.State

import Control.Monad.State
import Control.Optics.Pair
import public Control.Optics.Getter
import public Control.Optics.Lens
import public Control.Optics.Setter

export
use : MonadState s m => Getting a s a -> m a
use l = gets (view l)

export
data Zooming : (Type -> Type) -> Type -> Type -> Type where
  MkZooming : m (a, c) -> Zooming m c a

export
unZooming : Zooming m c a -> m (a, c)
unZooming (MkZooming x) = x

export
Monad m => Functor (Zooming m c) where
  map f (MkZooming m) = MkZooming (map (mapFst f) m)

export
zoom : (Monad m) => Simple (LensLike (Zooming m c)) s a -> StateT a m c -> StateT s m c
zoom l (ST m) = ST $ unZooming . applyMor (l (Mor (MkZooming . m)))

export
setting : MonadState s m => Setter s s a b -> b -> m ()
setting l b = modify (set l b)

export
modifying : MonadState s m => Setter s s a b -> (a -> b) -> m ()
modifying l f = modify (over l f)

infix 4 .=
export
(.=) : MonadState s m => Setter s s a b -> b -> m ()
(.=) = setting

infix 4 %=
export
(%=) : MonadState s m => Setter s s a b -> (a -> b) -> m ()
(%=) = modifying

infix 4 +=
export
(+=) : (MonadState s m, Num a) => Simple Setter s a -> a -> m ()
l += x = l %= (+ x)
