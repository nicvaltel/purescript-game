module RunGame
  ( runGame
  )
  where

import Prelude

import Concurrent.Queue as Q
import Config (Config)
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
gameStep wsMessages userInputs model = 
  model{gameStepNumber = model.gameStepNumber + 1}

mainLoop :: Config -> Q.Queue String -> Q.Queue UserInput -> Model -> Aff Unit
mainLoop conf queueWS queueInput model = do
  liftEffect $ render conf model -- TODO maybe change render to Aff ?
  delay $ Milliseconds 2000.0
  messages <- readAllQueue queueWS
  when conf.debug $ liftEffect $ log "MESSAGES:"
  when conf.debug $ liftEffect $ logShow messages
  inputs <- readAllQueue queueInput
  when conf.debug $ liftEffect $ log "INPUTS:"
  when conf.debug $ liftEffect $ logShow inputs
  let
    newModel = gameStep messages inputs model
  mainLoop conf queueWS queueInput newModel

runWS :: Config -> Q.Queue String -> Aff Unit
runWS conf queue = do
  sock <- liftEffect $ WS.initWebSocket conf.websocketUrl
  liftEffect $ WS.onOpen sock
  liftEffect $ WS.onClose sock
  liftEffect $ WS.onMessage sock
    $ \str -> do
        launchAff_ $ Q.write queue str

runGame :: Config -> Aff Unit
runGame conf = do --onDOMContentLoaded
  queueUserInput :: Q.Queue UserInput <- Q.new
  liftEffect $ runUserInput queueUserInput

  queueWS :: Q.Queue String <- Q.new
  runWS conf queueWS

  gameModel <- initGame
  mainLoop conf queueWS queueUserInput gameModel
  pure unit
