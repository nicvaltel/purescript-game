module Engine.GameLoop
  ( GameStepFunc
  , runGame
  ) where

import Engine.Reexport

import Concurrent.Queue as Q
import Control.Monad.State (evalStateT)
import Control.Monad.Trans.Class (lift)
import Data.Map as M
import Data.Traversable (sequence)
import Engine.Config (Config)
import Engine.Model (Actor(..), AppMod, AppModAff, AppModEffect, NameId, appModEffectToAppModAff, appModToAppModAff, getModelRec, getNameId, modmodAff, modmodEffect)
import Engine.Render.Render (render)
import Engine.ResourceLoader (getHtmlElement)
import Engine.Types (Time)
import Engine.UserInput (getUserInput)
import Engine.WebSocket.WSSignalChan as WS

newtype RequestAnimationFrameId
  = RequestAnimationFrameId Int

foreign import _requestAnimationFrame :: Effect Unit -> Effect RequestAnimationFrameId
foreign import _removeElementById :: String -> Effect Unit
foreign import _createImageElement :: 
  {canvasElem :: HTMLElement, x :: Number, y :: Number, imageSource :: String, divId :: String, cssClass :: String} 
  -> Effect HTMLElement

type GameStepFunc ac gm = 
  Config ac gm -> 
  Time -> 
  AppMod ac gm Unit


mainLoop :: forall ac gm. 
  Show gm => 
  Show ac => 
  Config ac gm -> 
  WS.WSocket -> 
  Q.Queue String -> 
  GameStepFunc ac gm -> 
  HTMLElement ->
  AppModAff ac gm Unit
mainLoop conf socket queueWS gameStep canvasElem = do
  model <- get
  renderFiber <- lift $ forkAff $ liftEffect (render conf model)
  currentTime <- liftEffect now
  seed <- liftEffect randomSeed
  let (Milliseconds deltaTime') = diff currentTime (getModelRec model).sys.lastUpdateTime
  let deltaTime = if deltaTime' < conf.maxDeltaTime then deltaTime' else conf.maxDeltaTime
  messages <- lift $ readAllQueue queueWS
  userInput <- liftEffect $ getUserInput canvasElem

  when conf.debugWebsocket $ when (not $ null messages) $ liftEffect do
    log "MESSAGES IN:"
    logShow messages
  when conf.debugUserInput $ liftEffect do
    log "INPUTS:"
    log $ show $ show userInput

  modmodAff $ \m -> m {io{userInput = userInput, prevUserInput = m.io.userInput, wsIn = messages, wsOut = []}, sys{seed = seed}}
  appModToAppModAff $ gameStep conf deltaTime 
  appModEffectToAppModAff $ updateRecentlyAddedActors canvasElem
  appModEffectToAppModAff $ removeRecentlyDeletedActors
  modmodAff $ \m -> m { sys{ lastUpdateTime = currentTime }}

  modelSendOut <- get
  liftEffect $ sendWsOutMessages socket (getModelRec modelSendOut).io.wsOut

  when conf.debugWebsocket $ when (not $ null (getModelRec modelSendOut).io.wsOut) $ liftEffect do
    log "MESSAGES OUT:"
    logShow (getModelRec modelSendOut).io.wsOut

  lift $ joinFiber renderFiber
  newModel <- get
  _ <- liftEffect $ _requestAnimationFrame (launchAff_ $ evalStateT (mainLoop conf socket queueWS gameStep canvasElem) newModel)
  pure unit

updateRecentlyAddedActors :: forall ac gm. HTMLElement -> AppModEffect ac gm Unit
updateRecentlyAddedActors canvasElem = do
  m <- getModelRec <$> get
  newActorsArr :: Array (Tuple NameId (Maybe HTMLElement)) <- liftEffect $ for m.act.recentlyAddedActors $ \nameId -> do
    maybeElem <- getHtmlElement (getNameId nameId)
    elem <- case maybeElem of
      Just el -> pure $ Just el
      Nothing -> sequence (createNewHtmlElem canvasElem <$> M.lookup nameId m.act.actors) 
    pure (Tuple nameId elem)
  let newActors = 
        foldr (\(Tuple nameId elem) acc -> M.update (\(Actor a) -> Just (Actor a{htmlElement = elem})) nameId acc) 
          m.act.actors 
          newActorsArr
  modmodEffect $ \mr -> mr{act{actors = newActors, recentlyAddedActors = []}}
  pure unit

createNewHtmlElem :: forall ac. HTMLElement -> Actor ac -> Effect HTMLElement
createNewHtmlElem canvasElem (Actor a) = _createImageElement {
  canvasElem : canvasElem, 
  x : a.x, 
  y : a.y, 
  imageSource : a.imageSource, 
  divId : getNameId a.nameId ,
  cssClass : a.cssClass
  }

removeRecentlyDeletedActors :: forall ac gm.  AppModEffect ac gm Unit
removeRecentlyDeletedActors = do
  m <- getModelRec <$> get
  _ <- liftEffect $ for m.act.recentlyDeletedActors $ \nameId -> do
    _removeElementById (getNameId nameId)
  modmodEffect $ \mr -> mr{act{recentlyDeletedActors = []}}

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
  AppModAff ac gm Unit
runGame conf gameStep = do --onDOMContentLoaded
  queueWS :: Q.Queue String <- lift Q.new
  socket <- lift $ runWS conf queueWS
  currentTime <- liftEffect now
  seed <- liftEffect randomSeed
  mbCanvasElem <- liftEffect $ getHtmlElement conf.canvasElementId
  canvasElem <- case mbCanvasElem of
          Just el -> pure el
          Nothing -> error ("ERROR: Canvas not found: canvasElementId = " <> conf.canvasElementId)
  modmodAff $ \m -> m { sys { lastUpdateTime = currentTime, seed = seed }}
  mainLoop conf socket queueWS gameStep canvasElem
