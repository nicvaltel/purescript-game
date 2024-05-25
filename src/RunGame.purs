module RunGame
  ( runGame
  )
  where

import Prelude

import Concurrent.Queue as Q
import Config (Config)
import Data.DateTime.Instant (Instant, diff, unInstant, instant)
import Data.Maybe (fromMaybe)
import Data.Time.Duration (Milliseconds(..), toDuration)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, forkAff, delay, launchAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Effect.Now (now)
import GameData as GD
import GameModel (Model, initGame)
import Graphics.Canvas (Dimensions)
import Render (render)
import ResourceLoader (loadImages)
import Types (Time)
import UserInput (UserInput, runUserInput)
import Utils.Utils (readAllQueue, undefined)
import WebSocket.WSSignalChan as WS

gameStep :: Time -> Array String -> Array UserInput -> Model -> Model
gameStep dt wsMessages userInputs model = 
  let model0 = GD.updateModel dt model
   in model0{gameStepNumber = model.gameStepNumber + 1}

mainLoop :: Config -> Q.Queue String -> Q.Queue UserInput -> Model -> Aff Unit
mainLoop conf queueWS queueInput model = do
  -- liftEffect (render conf model) 
  _ <- forkAff $ liftEffect (render conf model)
  currentTime <- liftEffect now
  let (Milliseconds deltaTime) = diff currentTime model.lastUpdateTime
      timeToWait = conf.frameRateNumber - deltaTime
  when (timeToWait > 0.0) $ delay (Milliseconds timeToWait)

  messages <- readAllQueue queueWS
  when conf.debug $ liftEffect $ log "MESSAGES:"
  when conf.debug $ liftEffect $ logShow messages
  inputs <- readAllQueue queueInput
  when conf.debug $ liftEffect $ log "INPUTS:"
  when conf.debug $ liftEffect $ logShow inputs
  let
    newModel0 = gameStep deltaTime messages inputs model
    newModel = newModel0{lastUpdateTime = currentTime}
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

  gameModel' <- liftEffect initGame
  spritesMap <- loadImages conf.images -- [{name : "red_ball", path : "images/1_Billiard_Ball@72p@72p.png"}]
  let gameModel = gameModel' {sprites = spritesMap}

  mainLoop conf queueWS queueUserInput gameModel
  pure unit
