module Engine.Utils.Utils where

import Prelude

import Concurrent.Queue as Q
import Data.Array ((:), reverse, head, filter)
import Data.Either (Either(..), either)
import Data.Enum (class Enum, enumFromTo)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Exception (throw)
import Effect.Unsafe (unsafePerformEffect)
import Unsafe.Coerce (unsafeCoerce)

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

error :: forall a. String -> a
error = unsafePerformEffect <<< throw

-- how to Trace :
-- let newASDF = trace ("HERE: " <> show x ) $ \_ -> 666
-- let newX = trace ("old X: " <> show x ) $ \_ -> x + 1
-- when (actor.nameId == "newBallActor") $ log (show $ delete (Proxy :: Proxy "elem") actorObj)