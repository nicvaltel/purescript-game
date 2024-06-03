module GameStep
  ( gameStep
  , gridCoords
  )
  where

import Prelude

import Control (ControlKey)
import Data.Array (filter, head, (:), range, unsafeIndex)
import Data.Int (round, toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Engine.Types (Time)
import Engine.UserInput (UserInput)
import Engine.Utils.Utils (undefined)
import Engine.WebSocket.WSSignalChan as WS
import GameModel (ConfigState, GameActor, GameModel, GameConfig)
import Partial.Unsafe (unsafePartial)
import Signal.DOM (CoordinatePair)

moveActor :: Time -> GameActor -> GameActor
moveActor dt actor =
  let
    newX = actor.x + dt * actor.state.vx

    newY = actor.y + dt * actor.state.vy
  in
    actor
      { x = if newX > 450.0 then newX - 450.0 else newX
      , y = if newY > 450.0 then newY - 450.0 else newY
      }

updateBlackHole :: ConfigState -> CoordinatePair -> GameActor -> GameActor
updateBlackHole conf mousePos ac = 
  let n = conf.gridNSize
      border = toNumber conf.boardBorderPixelSize
      gridArr = gridCoords n border (toNumber conf.boardPixelSize - border)
      x' = findNearestCoord mousePos.x n border gridArr
      y' = findNearestCoord mousePos.y n border gridArr
      offset = border - (conf.stoneScale * toNumber conf.stoneSpriteSize)/2.0
   in ac{x = x' + offset ,y = y' + offset}

gridCoords :: Int -> Number -> Number -> Array Number
gridCoords n x0 xEnd = 
  let xStep = (xEnd - x0) / (toNumber n)
  in map (\i -> (toNumber i) * xStep) (range 0 n)

findNearestCoord :: Int -> Int -> Number -> Array Number -> Number
findNearestCoord xMouse n borderSize xs = unsafePartial $
  let x = toNumber xMouse - borderSize
      xMin = xs `unsafeIndex` 0
      xMax = xs `unsafeIndex` n
  in find' x xMin xMax
  where 
    find' x xMin xMax
      | x < xMin = xMin
      | x > xMax = xMax
      | otherwise =
        let dx = (xMax - xMin)/(toNumber (n - 1)) 
            m = round $ (x - xMin)/dx 
         in toNumber m * dx


gameStep :: GameConfig -> Time -> Array WS.WSMessage -> Array (UserInput ControlKey) -> GameModel -> Tuple GameModel (Array String)
gameStep conf dt wsMessages userInputs model =
  let
    newUserInput = case head userInputs of
      Just input -> input
      Nothing -> model.prevUserInput
    newActors0 = filter (\a -> a.nameId /= "black_hole") $ map (moveActor dt) model.actors
    blackHole = fromMaybe undefined $ head $ filter (\a -> a.nameId == "black_hole") model.actors
    newActors =  (updateBlackHole conf.configState newUserInput.mouseRelativePos blackHole) : newActors0
    wsOut = wsMessages --[]
  in
    Tuple model { actors = newActors, gameStepNumber = model.gameStepNumber + 1, prevUserInput = newUserInput } wsOut