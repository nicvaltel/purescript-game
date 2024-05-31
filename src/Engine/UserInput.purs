module Engine.UserInput
  ( UserInput(..)
  , class Control
  , controlKeyMap
  , inverseControlKeyMap
  , runUserInput
  ) where

import Prelude
import Concurrent.Queue as Q
import Data.Array (catMaybes, filter, head, zip)
import Data.Enum (class Enum, enumFromTo)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Engine.Utils.Utils (undefined)
import Signal (Signal, runSignal)
import Signal.DOM (keyPressed)

type UserInput a
  = { keys :: Array a
    }

class
  (Bounded a, Enum a) <= Control a where
  controlKeyMap :: a -> Int

inverseControlKeyMap ∷ forall a. Control a => Int -> Maybe a
inverseControlKeyMap n = inverseMap controlKeyMap n

inverseMap ∷ forall a k. Bounded a => Enum a => Ord k => (a -> k) -> k -> Maybe a
inverseMap forwardMap k =
  head
    $ filter (\a -> forwardMap a == k)
    $ enumFromTo (bottom :: a) (top :: a)

getUserInput :: forall a. Control a => Array Int -> Effect (Signal (UserInput a))
getUserInput keysToListen = do
  signalKeyboard <- getInputKeyboard keysToListen
  pure $ (\ks -> { keys: catMaybes (map inverseControlKeyMap ks) }) <$> signalKeyboard

getInputKeyboard ∷ Array Int → Effect (Signal (Array Int))
getInputKeyboard keysToListen = do
  keysBoolSeqs <- traverse keyPressed keysToListen
  let
    keysBool = sequence keysBoolSeqs
  let
    maybeKeys = map (\bools -> map (\(Tuple n b) -> if b then Just n else Nothing) (zip keysToListen bools)) keysBool
  pure (catMaybes <$> maybeKeys)

runUserInput :: forall a. Control a => Q.Queue (UserInput a) -> Effect Unit
runUserInput queue = do
  userInputSignal <- getUserInput (mkKeysToListen (bottom :: a) (top :: a))
  runSignal (processUserInput <$> userInputSignal)
  where
  processUserInput :: UserInput a -> Effect Unit
  processUserInput = \n -> do
    launchAff_ $ Q.write queue n

  mkKeysToListen :: a -> a -> Array Int
  mkKeysToListen kmin kmax = map controlKeyMap $ enumFromTo kmin kmax

-- type AllInput
--   = { key :: Int
--     , time :: Time
--     , wsBuffer :: Array (Tuple Int String)
--     }
-- data SignalType
--   = FrameRateSignal Time
--   | UserInputSignal UserInput
--   | WebSocketSignal String
-- instance showSignalType :: Show SignalType where
--   show (FrameRateSignal time) = "FrameRateSignal" <> show time
--   show (UserInputSignal ui) = "UserInputSignal" <> show ui
--   show (WebSocketSignal msg) = "WebSocketSignal" <> show msg
-- frameRate :: Signal Time
-- frameRate = every constants.frameRateNumber
-- getUserInput :: Effect (Signal SignalType)
-- getUserInput = map (\k -> let n = if k then 32 else 0 in UserInputSignal { key: n }) <$> (keyPressed 32)
-- wsJoinedSignal :: Effect (Signal (Array (Tuple Int String)))
-- wsJoinedSignal = do
--   wsSig <- initWSSignal constants.websocketUrl
--   let
--     getJoinedInput = sampleOn frameRate (joinSignals 32 wsSig)
--   pure getJoinedInput
-- wsSignal :: Effect (Signal SignalType)
-- wsSignal = (map WebSocketSignal) <$> (initWSSignal constants.websocketUrl)
-- getAllInput :: Effect (Signal SignalType)
-- getAllInput = do
--   let frSig = FrameRateSignal <$> frameRate
--   uInputSig <- getUserInput
--   wsSig <- wsSignal
--   pure $ frSig <> uInputSig <> wsSig
-- -- getAllInput :: Effect (Signal AllInput)
-- -- getAllInput = do
-- --   uInput <- getUserInput
-- --   wsSig <- wsJoinedSignal
-- --   let xxx = uInput <> wsSig
-- --   pure $ map3 (\time input wsBuffer -> { key: input, time, wsBuffer }) frameRate uInput wsSig
