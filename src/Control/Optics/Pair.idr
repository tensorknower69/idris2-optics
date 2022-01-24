module Control.Optics.Pair

import public Control.Optics.Lens
import public Control.Optics.Getter
import public Control.Optics.Setter

export
lfst : Lens (a, c) (b, c) a b
lfst = lens fst (flip (mapFst . const))

export
lsnd : Lens (c, a) (c, b) a b
lsnd = lens snd (flip (mapSnd . const))

export
paired : {a, b : _} -> Simple Lens s a -> Simple Lens s b -> Simple Lens s (a, b)
paired l r = lens (\s => (view l s, view r s)) (\s, (a, b) => set l a $ set r b $ s)
