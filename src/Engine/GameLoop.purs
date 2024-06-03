module Engine.GameLoop
  ( GameStepFunc
  , runGame
  ) where

import Prelude

import Concurrent.Queue as Q
import Data.Array (head)
import Data.DateTime.Instant (diff)
import Data.Foldable (null, intercalate)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Effect.Now (now)
import Engine.Config (Config)
import Engine.Model (Model)
import Engine.Render.Render (render)
import Engine.Types (Time)
import Engine.UserInput (class Control, UserInput, runUserInput, showUserInput)
import Engine.Utils.Utils (readAllQueue)
import Engine.WebSocket.WSSignalChan as WS

newtype RequestAnimationFrameId
  = RequestAnimationFrameId Int

foreign import _requestAnimationFrame :: Effect Unit -> Effect RequestAnimationFrameId

type GameStepFunc ui gm ac cfg
  = Config cfg -> Time -> Array WS.WSMessage -> Array (UserInput ui) -> Model gm ac ui -> Tuple (Model gm ac ui) (Array String)

mainLoop :: forall ui gm ac cfg. Show ui => Show gm => Show ac => 
  Config cfg-> 
  WS.WSocket -> 
  Q.Queue String -> 
  Q.Queue (UserInput ui) -> 
  GameStepFunc ui gm ac cfg -> 
  Model gm ac ui-> 
  Aff Unit
mainLoop conf socket queueWS queueInput gameStep model = do
  -- _ <- forkAff $ liftEffect (render conf model)
  liftEffect (render conf model)
  currentTime <- liftEffect now
  let
    (Milliseconds deltaTime) = diff currentTime model.lastUpdateTime
  messages <- readAllQueue queueWS
  when conf.debugWebsocket $ when (not $ null messages) $ do
    liftEffect $ log "MESSAGES IN:"
    liftEffect $ logShow messages
  inputs <- readAllQueue queueInput
  when conf.debugUserInput $ when (not $ null inputs) $ liftEffect do
    log "INPUTS:"
    log $ show $ showUserInput <$> head inputs
  let
    (Tuple newModel' wsOut) = gameStep conf deltaTime messages inputs model
    newModel = newModel' { lastUpdateTime = currentTime }
  when conf.debugWebsocket $ when (not $ null wsOut) $ do
    liftEffect $ log "MESSAGES OUT:"
    liftEffect $ logShow wsOut
  liftEffect $ sendWsOutMessages socket wsOut
  _ <- liftEffect $ _requestAnimationFrame (launchAff_ $ mainLoop conf socket queueWS queueInput gameStep newModel)
  pure unit

sendWsOutMessages :: WS.WSocket -> Array String -> Effect Unit
sendWsOutMessages socket msgs = traverse_ (WS.sendMessage socket) msgs

runWS :: forall cfg. Config cfg -> Q.Queue String -> Aff WS.WSocket
runWS conf queue = do
  sock <- liftEffect $ WS.initWebSocket conf.websocketUrl
  liftEffect $ WS.onOpen sock
  liftEffect $ WS.onClose sock
  liftEffect $ WS.onMessage sock
    $ \str -> do
        launchAff_ $ Q.write queue str
  pure sock

runGame :: forall ui gm ac cfg. Control ui => Show gm => Show ac => 
  Config cfg-> 
  GameStepFunc ui gm ac cfg -> 
  Model gm ac ui -> 
  Aff Unit
runGame conf gameStep model = do --onDOMContentLoaded
  queueUserInput :: Q.Queue (UserInput ui) <- Q.new
  liftEffect $ runUserInput queueUserInput conf.canvasElementId
  queueWS :: Q.Queue String <- Q.new
  socket <- runWS conf queueWS
  currentTime <- liftEffect now
  let
    gameModel = model { lastUpdateTime = currentTime }
  mainLoop conf socket queueWS queueUserInput gameStep gameModel
  pure unit
