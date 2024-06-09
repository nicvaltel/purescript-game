module Bananan.GameStep
  ( gameStep
  )
  where

import Bananan.Reexport

import Bananan.Actors (ActorData(..), Ball, BallQueue, Dragon, Gun)
import Bananan.Control (ControlKey)
import Bananan.Control as C
import Bananan.GameModel (GameConfig, GameModel, GameActor)
import Engine.Model (Actor(..), Model(..))
import Engine.Types (Time)
import Engine.UserInput (UserInput)
import Engine.WebSocket.WSSignalChan as WS


moveBall :: Time -> Ball -> GameActor -> GameActor
moveBall dt ball ac@(Actor actor) = 
  case ball.flying of
    Nothing -> ac
    Just {vx,vy} ->
      let newX = actor.x + dt * vx
          newY = actor.y + dt * vy
      in Actor actor { x = newX , y = newY}

moveGun :: Time -> Array ControlKey -> Gun -> GameActor -> GameActor
moveGun dt controlKeys gun (Actor actor) = 
  let newSpeed = 
        let leftPressed = C.ArrowLeft `elem` controlKeys
            rightPressed = C.ArrowRight `elem` controlKeys
        in case Tuple leftPressed rightPressed of
            Tuple true false -> -gun.maxAngleSpeed
            Tuple false true -> gun.maxAngleSpeed
            _ -> 0.0
      newAngle' = actor.angle + newSpeed * dt
      newAngle = clamp gun.maxLeftAngle gun.maxRightAngle newAngle'
  in Actor actor{angle = newAngle, data = ActorGun gun{angleSpeed = newSpeed}}

moveDragon :: Time -> Dragon -> GameActor -> GameActor
moveDragon dt dragon actor = actor

moveBallQueue :: Time -> BallQueue -> GameActor -> GameActor
moveBallQueue dt queue actor = actor

moveActor :: Time -> UserInput -> Array ControlKey -> GameActor -> GameActor
moveActor dt userInput controlKeys ac@(Actor actor) = case actor.data of
  ActorBall ball -> moveBall dt ball ac
  ActorGun gun -> moveGun dt controlKeys gun ac
  ActorDragon dragon -> moveDragon dt dragon ac
  ActorBallQueue queue -> moveBallQueue dt queue ac

gameStep :: GameConfig -> Time -> Array WS.WSMessage -> UserInput -> GameModel -> Tuple GameModel (Array String)
gameStep conf dt wsMessages userInput (Model model) = 
  let 
      controlKeys = mapMaybe read userInput.keys
      newActors = map (moveActor dt userInput controlKeys) model.actors
      wsOut = wsMessages --[]
  in
    Tuple (Model model { actors = newActors, gameStepNumber = model.gameStepNumber + 1, prevUserInput = userInput }) wsOut