module Bananan.GameStep
  ( gameStep
  )
  where

import Bananan.Reexport

import Bananan.Actors (ActorData(..), Ball, BallQueue, Dragon, Gun)
-- import Bananan.Control (ControlKey)
-- import Bananan.Control as C
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

moveGun :: Time -> UserInput -> Gun -> GameActor -> GameActor
moveGun dt userInput gun (Actor actor) = 
  let 
      newSpeed = 
              -- case mbUserInput of
              --   Nothing -> 0.0
              --   Just userInput ->
                  -- let leftPressed = C.Left `elem` userInput.keys
                  --     rightPressed = C.Right `elem` userInput.keys
                  let leftPressed = "ArrowLeft" `elem` userInput.keys
                      rightPressed = "ArrowRight" `elem` userInput.keys
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

moveActor :: Time -> UserInput -> GameActor -> GameActor
moveActor dt userInput ac@(Actor actor) = case actor.data of
  ActorBall ball -> moveBall dt ball ac
  ActorGun gun -> moveGun dt  userInput gun ac
  ActorDragon dragon -> moveDragon dt dragon ac
  ActorBallQueue queue -> moveBallQueue dt queue ac

gameStep :: GameConfig -> Time -> Array WS.WSMessage -> UserInput -> GameModel -> Tuple GameModel (Array String)
gameStep conf dt wsMessages userInput (Model model) = 
  let newActors = map (moveActor dt userInput) model.actors
      wsOut = wsMessages --[]
  in
    Tuple (Model model { actors = newActors, gameStepNumber = model.gameStepNumber + 1, prevUserInput = userInput }) wsOut