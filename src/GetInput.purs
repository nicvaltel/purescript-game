module GetInput where

import Prelude
import Effect (Effect)
import Signal (Signal, sampleOn, map3)
import Signal.DOM (keyPressed)
import Signal.Time (Time)
import SignalM (frameRate, joinSignals)
import Data.Tuple (Tuple)
import WebSocket.WSSignalChan (initWSSignal)
import Constants (constants)

type AllInput
  = { key :: Int
    , time :: Time
    , wsBuffer :: Array (Tuple Int String)
    }

getUserInput :: Effect (Signal Int)
getUserInput = map (\k -> if k then 32 else 0) <$> (keyPressed 32)

wsJoinedSignal :: Effect (Signal (Array (Tuple Int String)))
wsJoinedSignal = do
  wsSig <- initWSSignal constants.websocketUrl
  let
    getJoinedInput = sampleOn frameRate (joinSignals 32 wsSig)
  pure getJoinedInput

getAllInput :: Effect (Signal AllInput)
getAllInput = do
  uInput <- getUserInput
  wsSig <- wsJoinedSignal
  pure $ map3 (\time input wsBuffer -> { key: input, time, wsBuffer }) frameRate uInput wsSig

-- getInput :: Effect (Signal AllInput)
-- getInput = do
--   -- The same is: (map2 (\t input -> {key : input, time : t}) frameRate) <$> getUserInput
--   uInput <- getUserInput
--   pure $ map2 (\t input -> { key: input, time: t }) frameRate uInput
