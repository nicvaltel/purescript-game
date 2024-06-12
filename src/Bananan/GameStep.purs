module Bananan.GameStep
  ( gameStep
  )
  where

import Bananan.Reexport

import Bananan.Actors (ActorData(..), BallQueueActor, Dragon, Gun, colorFromRandomInt, cssClassOfColor)
import Bananan.Control (ControlKey)
import Bananan.Control as C
import Bananan.GameModel (AppGame, GameActor, GameState)
import Data.Map as M
import Data.Number (abs, cos, pi, sin)
import Engine.GameLoop (GameStepFunc)
import Engine.Model (Actor(..), getModelRec, getRandom, mkNewNameId, modmod)
import Engine.Types (Time)
import Engine.UserInput (UserInput, keyWasPressedOnce)

type BoxWidth = Number

moveBall :: Time -> BoxWidth -> GameActor -> GameActor
moveBall dt width actor@(Actor a) =
  case a.data of
  (ActorBall ball) ->
    case ball.flying of
      Nothing -> actor
      Just {vx,vy} ->
        let newVx
              | a.x <= 0.0 = abs vx
              | a.x + a.width >= width = -(abs vx)
              | otherwise = vx
            newX = a.x + dt * newVx
            newY = let y' = a.y + dt * vy in if y' <= 0.0 then 0.0 else y' 
            newBall1 = if vx /= newVx then ball{flying = Just {vx : newVx, vy : vy}} else ball
            newBall2 = if newY <= 0.0 then newBall1{flying = Nothing} else newBall1
        in Actor a { x = newX , y = newY, data = ActorBall newBall2}
  _ -> actor

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
  let gun = case M.lookup m.gameState.gunNameId m.actors of
        Just (Actor actor)  -> actor
        Nothing -> error "There is no Gun actor in Model"
  let gunAngle = gun.angle
  let phi = pi * (90.0 - gunAngle)/180.0 
  let cosPhi = cos phi
  let sinPhi = sin phi
  let vx = cosPhi * m.gameState.ballSpeed
  let vy = - sinPhi * m.gameState.ballSpeed
  let ball = m.gameState.ballQueue{flying = Just {vx : vx, vy : vy}}

  let gunBottomX = gun.x + 25.0/2.0
  let gunBottomY = gun.y + 74.0
  let gunLenX = 74.0 * cosPhi
  let gunLenY = 74.0 * sinPhi
  let gunBarrelX = gunBottomX + gunLenX
  let gunBarrelY = gunBottomY - gunLenY

  nameId <- mkNewNameId
  randN :: Int <- getRandom
  let newQueueBall = {
          color : colorFromRandomInt randN
        , flying : Nothing
        }
  let newBallActor = Actor -- TODO it's just a mock
        {
          nameId : nameId
        , x : gunBarrelX - (76.0/2.0) -- 320.0 - (76.0/2.0)
        , y : gunBarrelY - (76.0/2.0) -- 800.0
        , width : 76.0
        , height : 76.0
        , z : 1
        , visible : true
        , angle : 0.0
        , cssClass : cssClassOfColor ball.color
        , imageSource : ""
        , htmlElement : Nothing
        , data : ActorBall ball
        }
  modmod $ \mr -> mr{
      actors = M.insert nameId newBallActor m.actors,
      recentlyAddedActors = nameId : m.recentlyAddedActors, 
      gameState = m.gameState{ballQueue = newQueueBall}
    }


moveDragon :: Time -> Dragon -> GameActor -> GameActor
moveDragon dt dragon actor = actor

moveBallQueue :: Time -> BallQueueActor -> GameActor -> GameActor
moveBallQueue dt queue actor = actor

moveActor :: Time -> BoxWidth -> UserInput -> Array ControlKey -> GameActor -> GameActor
moveActor dt width userInput controlKeys ac@(Actor actor) = case actor.data of
  ActorBall ball -> moveBall dt width ac
  ActorGun gun -> moveGun dt controlKeys gun ac
  ActorDragon dragon -> moveDragon dt dragon ac
  ActorBallQueue queue -> moveBallQueue dt queue ac


-- gameStep :: GameConfig -> Time -> AppMod GameConfig GameState Unit
gameStep :: GameStepFunc ActorData GameState
gameStep conf dt = do
  m <- getModelRec <$> get
  let controlKeys = mapMaybe read m.userInput.keys :: Array ControlKey
      prevControlKeys = mapMaybe read m.prevUserInput.keys :: Array ControlKey
      updatedActors = map (moveActor dt m.gameState.canvasWidth m.userInput controlKeys) m.actors
  modmod $ \mr -> m {actors = updatedActors}
  when (keyWasPressedOnce controlKeys prevControlKeys C.Space) fireBall
  let wsOut = m.wsIn --[]

  modmod $ \mr ->  mr { gameStepNumber = mr.gameStepNumber + 1, wsOut = wsOut }