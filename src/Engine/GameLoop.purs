module Engine.GameLoop
  ( GameStepFunc
  , runGame
  ) where

import Prelude
import Concurrent.Queue as Q
import Data.DateTime.Instant (diff)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Effect.Now (now)
import Engine.Config (Config)
import Engine.Model (Model)
import Engine.Render.Render (render)
import Engine.Types (Time)
import Engine.UserInput (UserInput, runUserInput)
import Engine.Utils.Utils (readAllQueue)
import Engine.WebSocket.WSSignalChan as WS

newtype RequestAnimationFrameId
  = RequestAnimationFrameId Int

foreign import _requestAnimationFrame :: Effect Unit -> Effect RequestAnimationFrameId

type GameStepFunc
  = Time -> Array String -> Array UserInput -> Model -> Model

mainLoop :: Config -> Q.Queue String -> Q.Queue UserInput -> GameStepFunc -> Model -> Aff Unit
mainLoop conf queueWS queueInput gameStep model = do
  -- _ <- forkAff $ liftEffect (render conf model)
  liftEffect (render conf model)
  currentTime <- liftEffect now
  let
    (Milliseconds deltaTime) = diff currentTime model.lastUpdateTime
  messages <- readAllQueue queueWS
  when conf.debug $ liftEffect $ log "MESSAGES:"
  when conf.debug $ liftEffect $ logShow messages
  inputs <- readAllQueue queueInput
  when conf.debug $ liftEffect $ log "INPUTS:"
  when conf.debug $ liftEffect $ logShow inputs
  let
    newModel0 = gameStep deltaTime messages inputs model

    newModel = newModel0 { lastUpdateTime = currentTime }
  _ <- liftEffect $ _requestAnimationFrame (launchAff_ $ mainLoop conf queueWS queueInput gameStep newModel)
  pure unit

runWS :: Config -> Q.Queue String -> Aff Unit
runWS conf queue = do
  sock <- liftEffect $ WS.initWebSocket conf.websocketUrl
  liftEffect $ WS.onOpen sock
  liftEffect $ WS.onClose sock
  liftEffect $ WS.onMessage sock
    $ \str -> do
        launchAff_ $ Q.write queue str

runGame :: Config -> GameStepFunc -> Model -> Aff Unit
runGame conf gameStep model = do --onDOMContentLoaded
  queueUserInput :: Q.Queue UserInput <- Q.new
  liftEffect $ runUserInput queueUserInput
  queueWS :: Q.Queue String <- Q.new
  runWS conf queueWS
  currentTime <- liftEffect now
  let
    gameModel = model { lastUpdateTime = currentTime }
  mainLoop conf queueWS queueUserInput gameStep gameModel
  pure unit
