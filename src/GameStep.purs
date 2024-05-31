module GameStep
  ( gameStep
  ) where

import Prelude
import Data.Tuple (Tuple(..))
import Engine.Model (Model, Actor)
import Engine.Types (Time)
import Engine.UserInput (class Control, UserInput)
import Engine.WebSocket.WSSignalChan as WS

moveActor :: Time -> Actor -> Actor
moveActor dt actor =
  let
    newX = actor.x + dt * actor.vx

    newY = actor.y + dt * actor.vy
  in
    actor
      { x = if newX > 1300.0 then newX - 1300.0 else newX
      , y = if newY > 800.0 then newY - 800.0 else newY
      }

gameStep :: forall a. Control a => Show a => Time -> Array WS.WSMessage -> Array (UserInput a) -> Model -> Tuple Model (Array String)
gameStep dt wsMessages userInputs model =
  let
    newActors = map (moveActor dt) model.actors

    wsOut = wsMessages --[]
  in
    Tuple model { actors = newActors, gameStepNumber = model.gameStepNumber + 1 } wsOut
