module Engine.GameLoop
  ( GameStepFunc
  , runGame
  ) where

import Engine.Reexport

import Concurrent.Queue as Q
import Control.Monad.State (evalStateT)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (maybe)
import Data.Traversable (for_, sequence)
import Engine.Config (Config)
import Engine.Model (class ActorContainer, Actor(..), AppMod, AppModAff, AppModEffect, NameId, appModEffectToAppModAff, appModToAppModAff, appModToAppModEffect, getModelRec, getNameId, lookupActor, modmodAff, modmodEffect, updateActor)
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
  Config -> 
  Time -> 
  AppMod ac gm Unit


mainLoop :: forall ac gm. 
  Show gm => 
  Show ac => 
  ActorContainer ac gm =>
  Config -> 
  WS.WSocket -> 
  Q.Queue String -> 
  GameStepFunc ac gm -> 
  HTMLElement ->
  AppModAff ac gm Unit
mainLoop conf socket queueWS gameStep canvasElem = do
  model <- get
  
  renderFiber <- lift $ forkAff $ liftEffect (render conf model)
  modmodAff $ \m -> m{audioElemsToPlay = []}

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
  
  -- GAME STEP
  appModToAppModAff $ gameStep conf deltaTime

  appModEffectToAppModAff $ updateRecentlyAddedActors
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

  where
    updateRecentlyAddedActors :: AppModEffect ac gm Unit
    updateRecentlyAddedActors = do
      model <- get
      let m = getModelRec model
      newActorsArr :: Array (Tuple NameId (Maybe HTMLElement)) <- liftEffect $ for m.act.recentlyAddedActors $ \addedActor -> do
        maybeParentElem <- getHtmlElement (getNameId addedActor.parentElemId)
        let parentElem = maybe canvasElem identity maybeParentElem
        maybeElem <- getHtmlElement (getNameId addedActor.nameId)
        elem <- case maybeElem of
          Just el -> pure $ Just el
          Nothing -> sequence (createNewHtmlElem parentElem <$> (lookupActor addedActor.nameId (Just addedActor.clue) model :: Maybe (Actor ac))) 
        pure (Tuple addedActor.nameId elem)
      appModToAppModEffect $ for_ newActorsArr $ \(Tuple nameId elem) -> 
        let update = updateActor :: NameId -> Maybe String -> (Actor ac -> Actor ac) -> AppMod ac gm Unit
        in update nameId Nothing (\(Actor a) -> Actor a{htmlElement = elem})
      modmodEffect $ \mr -> mr{act{recentlyAddedActors = []}}
      pure unit

createNewHtmlElem :: forall ac. HTMLElement -> Actor ac -> Effect HTMLElement
createNewHtmlElem parentElem (Actor a) = _createImageElement {
  canvasElem : parentElem, 
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

runWS ::  
  Config -> 
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
  ActorContainer ac gm =>
  Config -> 
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
