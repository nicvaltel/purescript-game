module RunGame where

import Prelude
import Concurrent.Queue as Q
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (Aff, forkAff, delay, launchAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import GameModel (Model, initGame)
import Render (render)
import UserInput (UserInput, runUserInput)
import Utils.Utils (readAllQueue)
import WebSocket.WSSignalChan as WS

gameStep :: Array String -> Array UserInput -> Model -> Model
gameStep wsMessages userInputs model = model

mainLoop :: Q.Queue String -> Q.Queue UserInput -> Model -> Aff Unit
mainLoop queueWS queueInput model = do
  liftEffect $ render model -- TODO maybe change render to Aff ?
  delay $ Milliseconds 2000.0
  messages <- readAllQueue queueWS
  liftEffect $ log "MESSAGES:"
  liftEffect $ logShow messages
  inputs <- readAllQueue queueInput
  liftEffect $ log "INPUTS:"
  liftEffect $ logShow inputs
  let
    newModel = gameStep messages inputs model
  mainLoop queueWS queueInput newModel

runWS :: Q.Queue String -> Aff Unit
runWS queue = do
  sock <- liftEffect $ WS.initWebSocket "ws://95.140.155.123:1234/ws"
  liftEffect $ WS.onOpen sock
  liftEffect $ WS.onClose sock
  liftEffect $ WS.onMessage sock
    $ \str -> do
        launchAff_ $ Q.write queue str

runGame :: Aff Unit
runGame = do --onDOMContentLoaded
  queueUserInput :: Q.Queue UserInput <- Q.new
  liftEffect $ runUserInput queueUserInput
  queueWS :: Q.Queue String <- Q.new
  runWS queueWS
  gameModel <- initGame
  mainLoop queueWS queueUserInput gameModel
  pure unit
