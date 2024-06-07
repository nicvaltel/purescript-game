module Engine.UserInput
  ( UserInput(..)
  , class Control
  , controlKeyMap
  , emptyUserInput
  , runUserInput
  , showUserInput
  )
  where

import Prelude
import Concurrent.Queue as Q
import Data.Array (catMaybes, zip)
import Data.Enum (class Enum, enumFromTo)
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Engine.Utils.Utils (inverseMap, undefined)
import Signal (Signal, map3, runSignal)
import Signal.DOM (CoordinatePair, MouseButton(..), keyPressed, mouseButtonPressed, mousePos)
import Engine.Utils.Html (getElementCoordinatesById)

foreign import _getElementCoord :: String -> Effect CoordinatePair

class
  (Bounded ui, Enum ui, Show ui) <= Control ui where
  controlKeyMap :: ui -> Int

type UserInput ui
  = { keys :: Array ui
    , mouseX :: Int
    , mouseY :: Int
    , mouseRelativePos :: CoordinatePair
    , mouseBtns :: Array MouseButton
    }

showUserInput :: forall ui. Show ui => UserInput ui -> String
showUserInput { keys, mouseX, mouseY, mouseRelativePos, mouseBtns } =
  "[{ "
    <> "keys: "
    <> show keys
    <> ", "
    <> "mouseX: "
    <> show mouseX
    <> ", "
    <> "mouseY: "
    <> show mouseY
    <> ", "
    <> "mouseRelativePos: "
    <> show mouseRelativePos
    <> ", "
    <> "mouseBtns: "
    <> (intercalate ", " $ map showMouseBtn mouseBtns)
    <> " }]"
  where
  showMouseBtn :: MouseButton -> String
  showMouseBtn MouseLeftButton = "MouseLeftButton"

  showMouseBtn MouseRightButton = "MouseRightButton"

  showMouseBtn MouseMiddleButton = "MouseMiddleButton"

  showMouseBtn MouseIE8MiddleButton = "MouseIE8MiddleButton"

emptyUserInput :: forall ui. UserInput ui
emptyUserInput = 
    { keys : []
    , mouseX : 0
    , mouseY : 0
    , mouseRelativePos : {x:0, y: 0}
    , mouseBtns : []
    }

getUserInput :: forall ui. Control ui => Array Int -> String -> Effect (Signal (UserInput ui))
getUserInput keysToListen canvasElementId = do
  signalKeyboard <- getInputKeyboard keysToListen
  mouseCoordSignal <- mousePos
  mouseBtnsSignal <- getInputMouseBtns
  mbCanvasPos <- getElementCoordinatesById canvasElementId
  let
    canvasPos = case mbCanvasPos of
      Just pos -> pos
      Nothing -> { x: 0, y: 0 }
  pure
    $ map3 (mkUserInput canvasPos)
        signalKeyboard
        mouseCoordSignal
        mouseBtnsSignal
  where
  mkUserInput :: CoordinatePair -> Array Int -> CoordinatePair -> Array MouseButton -> UserInput ui
  mkUserInput canvasPos ks mpos mbtns =
    { keys: catMaybes (map inverseControlKeyMap ks)
    , mouseX: mpos.x
    , mouseY: mpos.y
    , mouseRelativePos: { x: mpos.x - canvasPos.x, y: mpos.y - canvasPos.y }
    , mouseBtns: mbtns
    }

  inverseControlKeyMap ∷ Control ui => Int -> Maybe ui
  inverseControlKeyMap n = inverseMap controlKeyMap n

getInputMouseBtns :: Effect (Signal (Array MouseButton))
getInputMouseBtns = do
  let
    btnsToListen = [ MouseLeftButton, MouseRightButton, MouseMiddleButton, MouseIE8MiddleButton ]
  btnsBoolSeqs <- traverse mouseButtonPressed btnsToListen
  let
    keysBool = sequence btnsBoolSeqs
  let
    maybeBtns = map (\bools -> map (\(Tuple n b) -> if b then Just n else Nothing) (zip btnsToListen bools)) keysBool
  pure (catMaybes <$> maybeBtns)

getInputKeyboard ∷ Array Int → Effect (Signal (Array Int))
getInputKeyboard keysToListen = do
  keysBoolSeqs <- traverse keyPressed keysToListen
  let
    keysBool = sequence keysBoolSeqs
  let
    maybeKeys = map (\bools -> map (\(Tuple n b) -> if b then Just n else Nothing) (zip keysToListen bools)) keysBool
  pure (catMaybes <$> maybeKeys)

runUserInput :: forall ui. Control ui => Q.Queue (UserInput ui) -> String -> Effect Unit
runUserInput queue canvasElementId = do
  userInputSignal <- getUserInput (mkKeysToListen (bottom :: ui) (top :: ui)) canvasElementId
  runSignal (processUserInput <$> userInputSignal)
  where
  processUserInput :: UserInput ui -> Effect Unit
  processUserInput = \n -> do
    launchAff_ $ Q.write queue n

  mkKeysToListen :: ui -> ui -> Array Int
  mkKeysToListen kmin kmax = map controlKeyMap $ enumFromTo kmin kmax