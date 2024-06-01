module Engine.Utils.Utils where

import Prelude
import Unsafe.Coerce (unsafeCoerce)
import Concurrent.Queue as Q
import Effect.Aff (Aff)
import Data.Maybe (Maybe(..))
import Data.Array ((:), reverse, head, filter)
import Data.Either (Either(..), either)
import Data.Enum(class Enum, enumFromTo)

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

inverseMap ∷ forall a k. Bounded a => Enum a => Ord k => (a -> k) -> k -> Maybe a
inverseMap forwardMap k =
  head
    $ filter (\a -> forwardMap a == k)
    $ enumFromTo (bottom :: a) (top :: a)
