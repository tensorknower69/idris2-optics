module Control.Optics.Setter

import public Control.Optics.Types

export
sets : ((a -> b) -> s -> t) -> Setter s t a b
sets l (Mor f) = Mor $ Id . l (runIdentity . f)

export
mapped : Functor f => Setter (f a) (f b) a b
mapped = sets map

export
over : Setter s t a b -> (a -> b) -> s -> t
over optic f = runIdentity . applyMor (optic (Mor (Id . f)))

export
set : Setter s t a b -> b -> s -> t
set optic b = over optic (const b)

infixr 4 .~
export
(.~) : Setter s t a b -> b -> s -> t
(.~) = set

infixr 4 .~
export
(%~) : Setter s t a b -> (a -> b) -> s -> t
(%~) = over
