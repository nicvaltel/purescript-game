module GameLoop
  ( runGame
  )
  where

import Prelude

import Concurrent.Queue as Q
import Config (Config)
import Data.DateTime.Instant (diff)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Effect.Now (now)
import Model (Model, initGame)
import Render.Render (render)
import ResourceLoader (loadImages)
import Types (Time)
import UpdateModel (updateModel)
import UserInput (UserInput, runUserInput)
import Utils.Utils (readAllQueue)
import WebSocket.WSSignalChan as WS

newtype RequestAnimationFrameId = RequestAnimationFrameId Int
foreign import _requestAnimationFrame :: Effect Unit -> Effect RequestAnimationFrameId

gameStep :: Time -> Array String -> Array UserInput -> Model -> Model
gameStep dt wsMessages userInputs model = 
  let model0 = updateModel dt model
   in model0{gameStepNumber = model.gameStepNumber + 1}

mainLoop :: Config -> Q.Queue String -> Q.Queue UserInput -> Model -> Aff Unit
mainLoop conf queueWS queueInput model = do
  -- _ <- forkAff $ liftEffect (render conf model)
  liftEffect (render conf model)
  currentTime <- liftEffect now
  let (Milliseconds deltaTime) = diff currentTime model.lastUpdateTime

  messages <- readAllQueue queueWS
  when conf.debug $ liftEffect $ log "MESSAGES:"
  when conf.debug $ liftEffect $ logShow messages
  inputs <- readAllQueue queueInput
  when conf.debug $ liftEffect $ log "INPUTS:"
  when conf.debug $ liftEffect $ logShow inputs
  let
    newModel0 = gameStep deltaTime messages inputs model
    newModel = newModel0{lastUpdateTime = currentTime}
  _ <- liftEffect $ _requestAnimationFrame (launchAff_ $ mainLoop conf queueWS queueInput newModel)
  pure unit
  
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

  gameModel' <- liftEffect initGame
  spritesMap <- loadImages conf.images
  let gameModel = gameModel' {sprites = spritesMap}

  mainLoop conf queueWS queueUserInput gameModel
  pure unit
