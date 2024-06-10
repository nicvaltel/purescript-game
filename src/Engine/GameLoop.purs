module Engine.GameLoop
  ( GameStepFunc
  , runGame
  ) where

import Engine.Reexport

import Concurrent.Queue as Q
import Data.Map as M
import Engine.Config (Config)
import Engine.Model (Actor(..), Model(..))
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
  {canvasElem :: HTMLElement, x :: Number, y :: Number, imageSource :: String, divId :: String, css :: String} 
  -> Effect HTMLElement

type GameStepFunc ac gm = 
  Config ac gm -> 
  Time -> 
  Model ac gm -> 
  Model ac gm

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
  renderFiber <- forkAff $ liftEffect (render conf model) -- TODO make forkAff
  -- liftEffect (render conf model)
  currentTime <- liftEffect now
  let
    (Milliseconds deltaTime) = diff currentTime m.lastUpdateTime
  messages <- readAllQueue queueWS
  userInput <- liftEffect $ getUserInput canvasElem
  when conf.debugWebsocket $ when (not $ null messages) $ liftEffect do
    log "MESSAGES IN:"
    logShow messages
  when conf.debugUserInput $ liftEffect do
    log "INPUTS:"
    log $ show $ show userInput
  let
    modelWithInputs = Model m {userInput = userInput, prevUserInput = m.userInput, wsIn = messages, wsOut = []}
    newModel1 = gameStep conf deltaTime modelWithInputs
  newModel2 <- liftEffect $ updateRecentlyAddedActors canvasElem newModel1
  newModel3 <- liftEffect $ removeRecentlyDeletedActors newModel2
  let newModel = Model (unwrap newModel3) { lastUpdateTime = currentTime }
  liftEffect $ sendWsOutMessages socket (unwrap newModel).wsOut
  when conf.debugWebsocket $ when (not $ null (unwrap newModel).wsOut) $ liftEffect do
    log "MESSAGES OUT:"
    logShow (unwrap newModel).wsOut
  joinFiber renderFiber
  _ <- liftEffect $ _requestAnimationFrame (launchAff_ $ mainLoop conf socket queueWS gameStep canvasElem newModel)
  pure unit

updateRecentlyAddedActors :: forall ac gm. HTMLElement -> Model ac gm -> Effect (Model ac gm)
updateRecentlyAddedActors canvasElem (Model m) = do
  newActorsArr :: Array (Actor ac) <- for m.recentlyAddedActors $ \actor@(Actor a) -> do
    maybeElem <- getHtmlElement a.nameId
    elem <- case maybeElem of
      Just el -> pure el
      Nothing -> createNewHtmlElem canvasElem actor
    pure (Actor a{htmlElement = Just elem})
  let newActors = foldr (\a acc -> M.insert (unwrap a).nameId a acc) m.actors newActorsArr
  pure $ Model m{actors = newActors, recentlyAddedActors = []} -- TODO change from adding actors to modifying exsisted actors

createNewHtmlElem :: forall ac. HTMLElement -> Actor ac -> Effect HTMLElement
createNewHtmlElem canvasElem (Actor a)= _createImageElement {
  canvasElem : canvasElem, 
  x : a.x, 
  y : a.y, 
  imageSource : a.imageSource, 
  divId : a.nameId ,
  css : a.css
  }



removeRecentlyDeletedActors :: forall ac gm.  Model ac gm -> Effect (Model ac gm)
removeRecentlyDeletedActors (Model m) = do
  _ <- for m.recentlyDeletedActors $ \(Actor a) -> do
    _removeElementById a.nameId
  pure $ Model m{recentlyDeletedActors = []}

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
