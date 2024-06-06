module Bananan.GameStep
  ( gameStep
  )
  where

import Bananan.Reexport

import Bananan.Actors (ActorState(..), Ball, BallQueue, Dragon, Gun)
import Bananan.Control (ControlKey)
import Bananan.GameModel (GameActor, GameConfig, GameModel)
import Engine.Types (Time)
import Engine.UserInput (UserInput)
import Engine.WebSocket.WSSignalChan as WS


moveBall :: Time -> Ball -> GameActor -> GameActor
moveBall dt ball actor = 
  case ball.flying of
    Nothing -> actor
    Just {vx,vy} ->
      let newX = actor.x + dt * vx
          newY = actor.y + dt * vy
      in actor { x = newX , y = newY}

moveGun :: Time -> Gun -> GameActor -> GameActor
moveGun dt gun actor = actor

moveDragon :: Time -> Dragon -> GameActor -> GameActor
moveDragon dt dragon actor = actor

moveBallQueue :: Time -> BallQueue -> GameActor -> GameActor
moveBallQueue dt queue actor = actor

moveActor :: Time -> GameActor -> GameActor
moveActor dt actor = case actor.state of
  ActorBall ball -> moveBall dt ball actor
  ActorGun gun -> moveGun dt gun actor
  ActorDragon dragon -> moveDragon dt dragon actor
  ActorBallQueue queue -> moveBallQueue dt queue actor

gameStep :: GameConfig -> Time -> Array WS.WSMessage -> Array (UserInput ControlKey) -> GameModel -> Tuple GameModel (Array String)
gameStep conf dt wsMessages userInputs model =
  let
    newUserInput = case head userInputs of
      Just input -> input
      Nothing -> model.prevUserInput
    newActors = map (moveActor dt) model.actors
    wsOut = wsMessages --[]
  in
    Tuple model { actors = newActors, gameStepNumber = model.gameStepNumber + 1, prevUserInput = newUserInput } wsOut