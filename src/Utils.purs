module Utils where

import Prelude (unit)
import Signal.Channel (Channel)
import Unsafe.Coerce (unsafeCoerce)


undefined ∷ ∀ a. a
undefined = unsafeCoerce unit

