module Engine.GameLoop
  ( GameStepFunc
  , runGame
  ) where

import Prelude

import Concurrent.Queue as Q
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

type GameStepFunc a
  = Time -> Array WS.WSMessage -> Array (UserInput a) -> Model -> Tuple Model (Array String)

mainLoop :: forall a. Show a=> Config -> WS.WSocket -> Q.Queue String -> Q.Queue (UserInput a) -> GameStepFunc a -> Model -> Aff Unit
mainLoop conf socket queueWS queueInput gameStep model = do
  -- _ <- forkAff $ liftEffect (render conf model)
  liftEffect (render conf model)
  currentTime <- liftEffect now
  let
    (Milliseconds deltaTime) = diff currentTime model.lastUpdateTime
  messages <- readAllQueue queueWS
  when conf.debug $ liftEffect $ log "MESSAGES:"
  when conf.debug $ liftEffect $ logShow messages
  inputs <- readAllQueue queueInput
  when (not $ null inputs) $ liftEffect do
    log "INPUTS:"
    log $ intercalate "; " $ map showUserInput inputs
  when conf.debug $ liftEffect $ log "INPUTS:"
  when conf.debug $ liftEffect $ log $ intercalate "; " $ map showUserInput inputs
  let
    (Tuple newModel' wsOut) = gameStep deltaTime messages inputs model

    newModel = newModel' { lastUpdateTime = currentTime }
  liftEffect $ sendWsOutMessages socket wsOut
  _ <- liftEffect $ _requestAnimationFrame (launchAff_ $ mainLoop conf socket queueWS queueInput gameStep newModel)
  pure unit

sendWsOutMessages :: WS.WSocket -> Array String -> Effect Unit
sendWsOutMessages socket msgs = traverse_ (WS.sendMessage socket) msgs

runWS :: Config -> Q.Queue String -> Aff WS.WSocket
runWS conf queue = do
  sock <- liftEffect $ WS.initWebSocket conf.websocketUrl
  liftEffect $ WS.onOpen sock
  liftEffect $ WS.onClose sock
  liftEffect $ WS.onMessage sock
    $ \str -> do
        launchAff_ $ Q.write queue str
  pure sock

runGame :: forall a. Control a => Config -> GameStepFunc a -> Model -> Aff Unit
runGame conf gameStep model = do --onDOMContentLoaded
  queueUserInput :: Q.Queue (UserInput a) <- Q.new
  liftEffect $ runUserInput queueUserInput
  queueWS :: Q.Queue String <- Q.new
  socket <- runWS conf queueWS
  currentTime <- liftEffect now
  let
    gameModel = model { lastUpdateTime = currentTime }
  mainLoop conf socket queueWS queueUserInput gameStep gameModel
  pure unit
