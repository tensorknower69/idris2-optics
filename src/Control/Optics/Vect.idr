module Control.Optics.Vect

import Data.Vect
import Control.Optics.Setter
import Control.Optics.Lens

export
at : Fin n -> Simple Lens (Vect n a) a
at i = lens (index i) (\s, b => replaceAt i b s)
