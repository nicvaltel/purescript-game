module Bananan.GameStep
  ( gameStep
  )
  where

import Bananan.Reexport

-- import Bananan.Actors (ActorState(..), Ball, BallQueue, Dragon, Gun)
import Bananan.Control (ControlKey)
import Bananan.Control as C
import Bananan.GameModel (GameActor, GameConfig, GameModel)
import Engine.Types (Time)
import Engine.UserInput (UserInput)
import Engine.WebSocket.WSSignalChan as WS


-- moveBall :: Time -> Ball -> GameActor -> GameActor
-- moveBall dt ball actor = 
--   case ball.flying of
--     Nothing -> actor
--     Just {vx,vy} ->
--       let newX = actor.x + dt * vx
--           newY = actor.y + dt * vy
--       in actor { x = newX , y = newY}

-- moveGun :: Time -> Maybe (UserInput ControlKey) -> Gun -> GameActor -> GameActor
-- moveGun dt mbUserInput gun actor = 
--   let 
--       newSpeed = case mbUserInput of
--                 Nothing -> 0.0
--                 Just userInput ->
--                   let leftPressed = C.Left `elem` userInput.keys
--                       rightPressed = C.Right `elem` userInput.keys
--                   in case Tuple leftPressed rightPressed of
--                                   Tuple true false -> -gun.maxAngleSpeed
--                                   Tuple false true -> gun.maxAngleSpeed
--                                   _ -> 0.0
--       newAngle' = actor.angle + newSpeed * dt
--       newAngle = clamp gun.maxLeftAngle gun.maxRightAngle newAngle'
--   in actor{angle = newAngle, state = ActorGun gun{angleSpeed = newSpeed}}

-- moveDragon :: Time -> Dragon -> GameActor -> GameActor
-- moveDragon dt dragon actor = actor

-- moveBallQueue :: Time -> BallQueue -> GameActor -> GameActor
-- moveBallQueue dt queue actor = actor

-- moveActor :: Time -> Maybe (UserInput ControlKey) -> GameActor -> GameActor
-- moveActor dt mbUserInput actor = case actor.state of
--   ActorBall ball -> moveBall dt ball actor
--   ActorGun gun -> moveGun dt  mbUserInput gun actor
--   ActorDragon dragon -> moveDragon dt dragon actor
--   ActorBallQueue queue -> moveBallQueue dt queue actor

gameStep :: GameConfig -> Time -> Array WS.WSMessage -> Array (UserInput ControlKey) -> GameModel -> Tuple GameModel (Array String)
gameStep conf dt wsMessages userInputs model = 
  -- let
  --   newUserInput = case head userInputs of
  --     Just input -> input
  --     Nothing -> model.prevUserInput
  --   newActors = map (moveActor dt (head userInputs)) model.actors
  --   wsOut = wsMessages --[]
  -- in
  --   Tuple model { actors = newActors, gameStepNumber = model.gameStepNumber + 1, prevUserInput = newUserInput } wsOut
  Tuple model wsMessages