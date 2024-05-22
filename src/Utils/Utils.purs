module Utils.Utils where

import Prelude
import Unsafe.Coerce (unsafeCoerce)
import Concurrent.Queue as Q
import Effect.Aff (Aff)
import Data.Maybe (Maybe(..))
import Data.Array ((:), reverse)
import Data.Either (Either(..), either)

undefined ∷ ∀ a. a
undefined = unsafeCoerce unit

readAllQueue :: forall a. Q.Queue a -> Aff (Array a)
readAllQueue queue = reverse <$> readAllQueue' []
  where
  readAllQueue' :: Array a -> Aff (Array a)
  readAllQueue' accum = do
    element <- Q.tryRead queue
    case element of
      Just elem -> readAllQueue' (elem : accum)
      Nothing -> pure accum

mapLeft :: forall a b c. (a -> c) -> Either a b -> Either c b
mapLeft f = either (Left <<< f) Right
