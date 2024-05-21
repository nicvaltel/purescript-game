module GetInput where

import Prelude
import Constants (constants)
import Data.Tuple (Tuple)
import Effect (Effect)
import Signal (Signal, sampleOn, map3)
import Signal.DOM (keyPressed)
import Signal.Time (Time, every)
import SignalM (joinSignals)
import Utils (undefined)
import WebSocket.WSSignalChan (initWSSignal)

type AllInput
  = { key :: Int
    , time :: Time
    , wsBuffer :: Array (Tuple Int String)
    }

type UserInput
  = { key :: Int
    }

data SignalType
  = FrameRateSignal Time
  | UserInputSignal UserInput
  | WebSocketSignal String

instance showSignalType :: Show SignalType where
  show (FrameRateSignal time) = "FrameRateSignal" <> show time
  show (UserInputSignal ui) = "UserInputSignal" <> show ui
  show (WebSocketSignal msg) = "WebSocketSignal" <> show msg

frameRate :: Signal Time
frameRate = every constants.frameRateNumber

getUserInput :: Effect (Signal SignalType)
getUserInput = map (\k -> let n = if k then 32 else 0 in UserInputSignal { key: n }) <$> (keyPressed 32)

wsJoinedSignal :: Effect (Signal (Array (Tuple Int String)))
wsJoinedSignal = do
  wsSig <- initWSSignal constants.websocketUrl
  let
    getJoinedInput = sampleOn frameRate (joinSignals 32 wsSig)
  pure getJoinedInput

wsSignal :: Effect (Signal SignalType)
wsSignal = (map WebSocketSignal) <$> (initWSSignal constants.websocketUrl)

getAllInput :: Effect (Signal SignalType)
getAllInput = do
  let frSig = FrameRateSignal <$> frameRate
  uInputSig <- getUserInput
  wsSig <- wsSignal
  pure $ frSig <> uInputSig <> wsSig

-- getAllInput :: Effect (Signal AllInput)
-- getAllInput = do
--   uInput <- getUserInput
--   wsSig <- wsJoinedSignal
--   let xxx = uInput <> wsSig
--   pure $ map3 (\time input wsBuffer -> { key: input, time, wsBuffer }) frameRate uInput wsSig
