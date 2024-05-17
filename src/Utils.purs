module Utils where

import Unsafe.Coerce (unsafeCoerce)
import Prelude (unit)


undefined ∷ ∀ a. a
undefined = unsafeCoerce unit

