module Engine.UserInput
  ( UserInput(..)
  , class Control
  , controlKeyMap
  , inverseControlKeyMap
  , runUserInput
  , showUserInput
  )
  where

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
import Signal (Signal, runSignal, map3)
import Signal.DOM (CoordinatePair(..), MouseButton(..), keyPressed, mouseButton, mouseButtonPressed, mousePos)
import Data.Foldable(intercalate)

type UserInput a
  = { keys :: Array a,
      mouseX :: Int,
      mouseY :: Int,
      mouseBtns :: Array MouseButton
    }

showUserInput :: forall a. Show a => UserInput a -> String
showUserInput {keys, mouseX, mouseY, mouseBtns} = 
    "[{ " <>
    "keys: " <> show keys <> ", " <>
    "mouseX: " <> show mouseX <> ", " <>
    "mouseY: " <> show mouseY <> ", " <>
    "mouseBtns: " <> (intercalate ", " $ map showMouseBtn mouseBtns )
    <> " }]"
    where
      showMouseBtn :: MouseButton -> String
      showMouseBtn MouseLeftButton = "MouseLeftButton"
      showMouseBtn MouseRightButton = "MouseRightButton"
      showMouseBtn MouseMiddleButton = "MouseMiddleButton"
      showMouseBtn MouseIE8MiddleButton = "MouseIE8MiddleButton"

class
  (Bounded a, Enum a, Show a) <= Control a where
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
  mouseCoord <- mousePos
  mouseBtns <- getInputMouseBtns
  pure $ 
    map3 mkUserInput
      signalKeyboard 
      mouseCoord
      mouseBtns
  where
    mkUserInput ks mpos mbtns = 
            { 
              keys: catMaybes (map inverseControlKeyMap ks), 
              mouseX : mpos.x, 
              mouseY : mpos.y,
              mouseBtns : mbtns 
            }
  -- pure $ (\ks -> { keys: catMaybes (map inverseControlKeyMap ks), mouseX : 0, mouseY : 0 }) <$> signalKeyboard

getInputMouseBtns :: Effect (Signal (Array MouseButton))
getInputMouseBtns = do
  let btnsToListen = [MouseLeftButton, MouseRightButton, MouseMiddleButton, MouseIE8MiddleButton]
  btnsBoolSeqs <- traverse mouseButtonPressed btnsToListen
  let
    keysBool = sequence btnsBoolSeqs
  let
    maybeBtns = map (\bools -> map (\(Tuple n b) -> if b then Just n else Nothing) (zip btnsToListen bools)) keysBool
  pure (catMaybes <$> maybeBtns)

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
