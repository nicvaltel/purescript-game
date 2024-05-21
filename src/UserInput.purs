module UserInput
  ( UserInput(..)
  , runUserInput
  )
  where

import Prelude
import Effect (Effect)
import Signal (Signal, runSignal)
import Signal.DOM (keyPressed)
import Utils.Utils (undefined)
import Concurrent.Queue as Q
import Effect.Aff (launchAff_)


type UserInput
  = { key :: Int
    }

getUserInput :: Effect (Signal UserInput)
getUserInput = map (\k -> let n = if k then 32 else 0 in { key: n }) <$> (keyPressed 32)

runUserInput :: Q.Queue UserInput -> Effect Unit
runUserInput queue = do
    userInputSignal <- getUserInput
    runSignal (processUserInput <$> userInputSignal) 
    where
        processUserInput :: UserInput -> Effect Unit
        processUserInput = \n -> do 
            launchAff_ $ Q.write queue n




















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
