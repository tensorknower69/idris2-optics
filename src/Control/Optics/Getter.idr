module Control.Optics.Getter

import public Control.Optics.Types

export
view : Getting a s a -> s -> a
view l = runConst . applyMor (l (Mor MkConst))

infixl 8 ^.
export
(^.) : s -> Getting a s a -> a
s ^. l = view l s

export
to : (Profunctor p, Contravariant f) => (s -> a) -> Simple (Optic p f) s a
to k = dimap k (contramap k)
