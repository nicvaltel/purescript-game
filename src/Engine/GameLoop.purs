module Engine.GameLoop
  ( GameStepFunc
  , runGame
  ) where

import Prelude

import Concurrent.Queue as Q
import Data.Array (head)
import Data.DateTime.Instant (diff)
import Data.Foldable (null)
import Data.Maybe (Maybe(..), maybe)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Effect.Exception (throwException)
import Effect.Now (now)
import Engine.Config (Config)
import Engine.Model (Model(..))
import Engine.Render.Render (render)
import Engine.ResourceLoader (getHtmlElement)
import Engine.Types (Time)
import Engine.UserInput (UserInput(..), getUserInput)
import Engine.Utils.Utils (error, readAllQueue, undefined)
import Engine.WebSocket.WSSignalChan as WS
import Unsafe.Coerce (unsafeCoerce)
import Web.HTML (HTMLElement)

newtype RequestAnimationFrameId
  = RequestAnimationFrameId Int

foreign import _requestAnimationFrame :: Effect Unit -> Effect RequestAnimationFrameId
type GameStepFunc ac gm = 
  Config ac gm -> 
  Time -> 
  Array WS.WSMessage -> 
  UserInput -> 
  Model ac gm -> 
  Tuple (Model ac gm) (Array String) -- TODO cahnge Tuple to output type

mainLoop :: forall ac gm. 
  Show gm => 
  Show ac => 
  Config ac gm -> 
  WS.WSocket -> 
  Q.Queue String -> 
  GameStepFunc ac gm -> 
  HTMLElement ->
  Model ac gm -> 
  Aff Unit
mainLoop conf socket queueWS gameStep canvasElem model@(Model m) = do
  -- _ <- forkAff $ liftEffect (render conf model) -- TODO make forkAff
  liftEffect (render conf model)
  currentTime <- liftEffect now
  let
    (Milliseconds deltaTime) = diff currentTime m.lastUpdateTime
  messages <- readAllQueue queueWS
  when conf.debugWebsocket $ when (not $ null messages) $ liftEffect do
    log "MESSAGES IN:"
    logShow messages
  userInput <- liftEffect $ getUserInput canvasElem
  when conf.debugUserInput $ liftEffect do
    log "INPUTS:"
    log $ show $ show userInput
  let
    (Tuple (Model newModel') wsOut) = gameStep conf deltaTime messages userInput model
    newModel = Model newModel' { lastUpdateTime = currentTime }
  when conf.debugWebsocket $ when (not $ null wsOut) $ do
    liftEffect $ log "MESSAGES OUT:"
    liftEffect $ logShow wsOut
  liftEffect $ sendWsOutMessages socket wsOut
  _ <- liftEffect $ _requestAnimationFrame (launchAff_ $ mainLoop conf socket queueWS gameStep canvasElem newModel)
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



runGame :: forall ac gm. 
  Show gm => 
  Show ac => 
  Config ac gm -> 
  GameStepFunc ac gm -> 
  Model ac gm -> 
  Aff Unit
runGame conf gameStep (Model model) = do --onDOMContentLoaded
  queueWS :: Q.Queue String <- Q.new
  socket <- runWS conf queueWS
  currentTime <- liftEffect now
  mbCanvasElem <- liftEffect $ getHtmlElement conf.canvasElementId
  canvasElem <- case mbCanvasElem of
          Just el -> pure el
          Nothing -> error ("ERROR: Canvas not found: canvasElementId = " <> conf.canvasElementId)
  let  gameModel = Model model { lastUpdateTime = currentTime}
  mainLoop conf socket queueWS gameStep canvasElem gameModel
  pure unit
