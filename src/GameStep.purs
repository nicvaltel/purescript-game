module GameStep
  ( gameStep
  ) where

import Prelude
import Engine.Types (Time)
import Engine.UserInput (UserInput)
import Engine.Model (Model, Actor)

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

gameStep :: Time -> Array String -> Array UserInput -> Model -> Model
gameStep dt wsMessages userInputs model =
  let
    newActors = map (moveActor dt) model.actors
  in
    model { actors = newActors, gameStepNumber = model.gameStepNumber + 1 }
