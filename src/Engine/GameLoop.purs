module Engine.GameLoop
  ( GameStepFunc
  , runGame
  ) where

import Prelude

import Concurrent.Queue as Q
import Data.Array (head)
import Data.DateTime.Instant (diff)
import Data.Foldable (null)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Effect.Now (now)
import Engine.Config (Config)
import Engine.Model (Model(..))
import Engine.Render.Render (render)
import Engine.Types (Time)
import Engine.UserInput (class Control, UserInput, runUserInput, showUserInput)
import Engine.Utils.Utils (readAllQueue)
import Engine.WebSocket.WSSignalChan as WS

newtype RequestAnimationFrameId
  = RequestAnimationFrameId Int

foreign import _requestAnimationFrame :: Effect Unit -> Effect RequestAnimationFrameId
type GameStepFunc ac gm ui = 
  Config ac gm -> 
  Time -> 
  Array WS.WSMessage -> 
  Array (UserInput ui) -> 
  Model ac gm ui -> 
  Tuple (Model ac gm ui) (Array String)

mainLoop :: forall ac gm ui. 
  Show ui => 
  Show gm => 
  Show ac => 
  Config ac gm -> 
  WS.WSocket -> 
  Q.Queue String -> 
  Q.Queue (UserInput ui) -> 
  GameStepFunc ac gm ui -> 
  Model ac gm ui -> 
  Aff Unit
mainLoop conf socket queueWS queueInput gameStep model@(Model m) = do
  -- _ <- forkAff $ liftEffect (render conf model) -- TODO make forkAff
  liftEffect (render conf model)
  currentTime <- liftEffect now
  let
    (Milliseconds deltaTime) = diff currentTime m.lastUpdateTime
  messages <- readAllQueue queueWS
  when conf.debugWebsocket $ when (not $ null messages) $ do
    liftEffect $ log "MESSAGES IN:"
    liftEffect $ logShow messages
  inputs <- readAllQueue queueInput
  when conf.debugUserInput $ when (not $ null inputs) $ liftEffect do
    log "INPUTS:"
    log $ show $ showUserInput <$> head inputs
  let
    (Tuple (Model newModel') wsOut) = gameStep conf deltaTime messages inputs model
    newModel = Model newModel' { lastUpdateTime = currentTime }
  when conf.debugWebsocket $ when (not $ null wsOut) $ do
    liftEffect $ log "MESSAGES OUT:"
    liftEffect $ logShow wsOut
  liftEffect $ sendWsOutMessages socket wsOut
  _ <- liftEffect $ _requestAnimationFrame (launchAff_ $ mainLoop conf socket queueWS queueInput gameStep newModel)
  pure unit

sendWsOutMessages :: WS.WSocket -> Array String -> Effect Unit
sendWsOutMessages socket msgs = traverse_ (WS.sendMessage socket) msgs

runWS :: forall ac gm.  
  Config ac gm -> 
  Q.Queue String -> 
  Aff WS.WSocket
runWS conf queue = do
  sock <- liftEffect $ WS.initWebSocket conf.websocketUrl
  liftEffect $ WS.onOpen sock
  liftEffect $ WS.onClose sock
  liftEffect $ WS.onMessage sock
    $ \str -> do
        launchAff_ $ Q.write queue str
  pure sock



runGame :: forall ac gm ui. 
  Control ui => 
  Show gm => 
  Show ac => 
  Config ac gm -> 
  GameStepFunc ac gm ui -> 
  Model ac gm ui -> 
  Aff Unit
runGame conf gameStep (Model model) = do --onDOMContentLoaded
  queueUserInput :: Q.Queue (UserInput ui) <- Q.new
  liftEffect $ runUserInput queueUserInput conf.canvasElementId
  queueWS :: Q.Queue String <- Q.new
  socket <- runWS conf queueWS
  currentTime <- liftEffect now
  let
    gameModel = Model model { lastUpdateTime = currentTime}
  mainLoop conf socket queueWS queueUserInput gameStep gameModel
  pure unit
