module GameStep
  ( gameStep
  ) where

import Prelude

import Data.Tuple (Tuple(..))
import Engine.Model (Model, Actor)
import Engine.Types (Time)
import Engine.UserInput (class Control, UserInput)
import Engine.WebSocket.WSSignalChan as WS
import InitGame (ActorState, GameModel, GameState, GameActor)

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

gameStep :: forall ui. Control ui => Time -> Array WS.WSMessage -> Array (UserInput ui) -> GameModel -> Tuple GameModel (Array String)
gameStep dt wsMessages userInputs model =
  let
    newActors = map (moveActor dt) model.actors
    wsOut = wsMessages --[]
    -- wsOut = 
    --   if userInputs /= []
    --     then [show userInputs] <> wsMessages
    --     else wsMessages --[]
  in
    Tuple model { actors = newActors, gameStepNumber = model.gameStepNumber + 1 } wsOut