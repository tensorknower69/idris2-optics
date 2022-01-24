module Control.Optics.Types

import public Control.Applicative.Const
import public Control.Monad.Identity
import public Data.Contravariant
import public Data.Functor.Tagged
import public Data.Morphisms
import public Data.Profunctor
import public Data.Profunctor.Choice

public export
Simple : (Type -> Type -> Type -> Type -> Type) -> Type -> Type -> Type
Simple p s a = p s s a a

public export
Optical : (p : Type -> Type -> Type)
       -> (q : Type -> Type -> Type)
       -> (f : Type -> Type)
       -> (s : Type)
       -> (t : Type)
       -> (a : Type)
       -> (b : Type)
       -> Type
Optical p q f s t a b = p a (f b) -> q s (f t)

public export
Optic : (Type -> Type -> Type) -> (Type -> Type) -> Type -> Type -> Type -> Type -> Type
Optic p = Optical p p

public export
LensLike : (Type -> Type) -> Type -> Type -> Type -> Type -> Type
LensLike = Optic Morphism

public export
Lens : Type -> Type -> Type -> Type -> Type
Lens s t a b = {f : Type -> Type} -> Functor f => LensLike f s t a b

public export
Setter : Type -> Type -> Type -> Type -> Type
Setter s t a b = LensLike Identity s t a b

public export
Getter : Type -> Type -> Type
Getter s a = {f : _} -> (Contravariant f, Functor f) => Simple (LensLike f) s a

public export
Getting : Type -> Type -> Type -> Type
Getting r s a = Simple (LensLike (Const r)) s a

public export
Prism : Type -> Type -> Type -> Type -> Type
Prism s t a b = {p, f : _} -> (Choice p, Applicative f) => Optic p f s t a b

public export
Review : Type -> Type -> Type -> Type -> Type
Review s t a b = {p : _} -> (Choice p) => Optic p Identity s t a b

public export
AReview : Type -> Type -> Type
AReview t b = Simple (Optic Tagged Identity) t b
