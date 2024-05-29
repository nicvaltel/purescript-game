module Engine.SkipFrames.SkipFrames where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Signal (Signal, runSignal, foldp)
import Signal.Time (every, Time, now)
import Data.Array (range)
import Data.Foldable (sum)
import Effect.Ref (Ref, new, read, write)

type Model
  = { prevTime :: Time
    , step :: Int
    }

initialModel :: Model
initialModel =
  { prevTime: 0.0
  , step: 0
  }

frameRate :: Signal Time
frameRate = every 1000.0

processLogic :: Time -> Model -> Model
processLogic time m = m { step = m.step + 1 }

processSignal :: Signal Model
processSignal = foldp processLogic initialModel frameRate

render :: Ref Time -> Model -> Effect Unit
render refTime model = do
  currentTime <- now
  prevTime <- read refTime
  if (currentTime - prevTime) < 1500.0 then do
    write currentTime refTime
    log $ "current time: " <> show currentTime
    log $ "prev time: " <> show prevTime
    let
      n = model.step
    let
      arr = range n (n + 100_000_000)
    log $ (show n) <> ":\t" <> (show $ sum arr)
    log (show model)
  else
    log "SKIP"
  pure unit

runExample :: Effect Unit
runExample = do
  currentTime <- now
  time <- new currentTime
  runSignal (render time <$> processSignal)

-- mainLoop :: Config -> Q.Queue String -> Q.Queue UserInput -> Model -> Aff Unit
-- mainLoop conf queueWS queueInput model = do
--   -- liftEffect (render conf model) 
--   _ <- forkAff $ liftEffect (render conf model)
--   currentTime <- liftEffect now
--   let (Milliseconds deltaTime) = diff currentTime model.lastUpdateTime
--       timeToWait = conf.frameRateNumber - deltaTime
--   when (timeToWait > 0.0) $ delay (Milliseconds timeToWait)
--   messages <- readAllQueue queueWS
--   when conf.debug $ liftEffect $ log "MESSAGES:"
--   when conf.debug $ liftEffect $ logShow messages
--   inputs <- readAllQueue queueInput
--   when conf.debug $ liftEffect $ log "INPUTS:"
--   when conf.debug $ liftEffect $ logShow inputs
--   let
--     newModel0 = gameStep deltaTime messages inputs model
--     newModel = newModel0{lastUpdateTime = currentTime}
--   mainLoop conf queueWS queueInput newModel
