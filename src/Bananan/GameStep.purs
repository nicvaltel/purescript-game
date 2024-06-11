module Bananan.GameStep
  ( gameStep
  )
  where

import Bananan.Reexport
import Bananan.Actors (ActorData(..), Ball, BallQueueActor, Dragon, Gun, colorFromRandomInt, cssClassOfColor)
import Bananan.Control (ControlKey)
import Bananan.Control as C
import Bananan.GameModel (AppGame, GameActor, GameState)
import Data.Map as M
import Engine.GameLoop (GameStepFunc)
import Engine.Model (Actor(..), getModelRec, mkNewNameId, modmod)
import Engine.Types (Time)
import Engine.UserInput (UserInput, keyWasPressedOnce)


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

fireBall :: AppGame Unit
fireBall = do
  m <- getModelRec <$> get
  nameId <- mkNewNameId
  let randomPair = random m.seed :: RandomPair Int -- TODO make random via modify Model
  let ball = m.gameState.ballQueue{flying = Just {vx : 0.05, vy : -0.05}}
  let newQueueBall = {
          color : colorFromRandomInt randomPair.newVal
        , flying : Nothing
        }
  let newBallActor = Actor -- TODO it's just a mock
        {
          nameId : nameId
        , x : 300.0
        , y : 300.0
        , z : 1
        , visible : true
        , angle : 0.0
        , cssClass : cssClassOfColor ball.color
        , imageSource : "../images/ball.png"
        , htmlElement : Nothing
        , data : ActorBall ball
        }
  modmod $ \mr -> mr{
      actors = M.insert nameId newBallActor m.actors,
      recentlyAddedActors = nameId : m.recentlyAddedActors, 
      gameState = m.gameState{ballQueue = newQueueBall},
      seed = randomPair.newSeed 
    }


moveDragon :: Time -> Dragon -> GameActor -> GameActor
moveDragon dt dragon actor = actor

moveBallQueue :: Time -> BallQueueActor -> GameActor -> GameActor
moveBallQueue dt queue actor = actor

moveActor :: Time -> UserInput -> Array ControlKey -> GameActor -> GameActor
moveActor dt userInput controlKeys ac@(Actor actor) = case actor.data of
  ActorBall ball -> moveBall dt ball ac
  ActorGun gun -> moveGun dt controlKeys gun ac
  ActorDragon dragon -> moveDragon dt dragon ac
  ActorBallQueue queue -> moveBallQueue dt queue ac



-- gameStep :: GameConfig -> Time -> AppMod GameConfig GameState Unit
gameStep :: GameStepFunc ActorData GameState
gameStep conf dt = do
  m <- getModelRec <$> get
  let controlKeys = mapMaybe read m.userInput.keys :: Array ControlKey
      prevControlKeys = mapMaybe read m.prevUserInput.keys :: Array ControlKey
      updatedActors = map (moveActor dt m.userInput controlKeys) m.actors
  modmod $ \mr -> m {actors = updatedActors}
  when (keyWasPressedOnce controlKeys prevControlKeys C.Space) fireBall
  let wsOut = m.wsIn --[]

  modmod $ \mr ->  mr { gameStepNumber = mr.gameStepNumber + 1, wsOut = wsOut }