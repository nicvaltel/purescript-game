module Bananan.GameStep
  ( gameStep
  )
  where

import Bananan.Reexport

import Bananan.Actors (ActorData(..), BallQueueActor, Dragon, colorFromRandomInt, cssClassOfColor)
import Bananan.Control (ControlKey)
import Bananan.Control as C
import Bananan.GameModel (AppGame, GameActor, GameState(..), getGameRec)
import Data.List (List)
import Data.List as List
import Data.Map as M
import Data.Maybe (isNothing)
import Data.Number (abs, cos, pi, sin, sqrt)
import Engine.GameLoop (GameStepFunc)
import Engine.Model (Actor(..), getModelRec, getRandom, mkNewNameId, modmod)
import Engine.Types (Time)
import Engine.UserInput (keyWasPressedOnce)

type BoxWidth = Number
type Diameter = Number

ballsIntersection :: forall ac. Diameter -> Actor ac -> List (Actor ac) -> List (Actor ac)
ballsIntersection d (Actor ball) = List.filter predicate
  where 
    d2 = d*d
    predicate (Actor b) = 
      let dx = ball.x - b.x 
          dy = ball.y - b.y
       in if abs (ball.x - b.x) < d && abs (ball.y - b.y) < d
          then dx * dx + dy * dy <= d2
          else false


correctBallPosition :: forall ac. Diameter -> Actor ac -> List (Actor ac) -> Actor ac 
correctBallPosition d ball balls = foldr correctBallPositionOnce ball balls
  where
    ballsDistance :: Actor ac -> Actor ac -> Number
    ballsDistance (Actor c1) (Actor c2) =
      let
        dx = c1.x - c2.x
        dy = c1.y - c2.y
      in
        sqrt (dx * dx + dy * dy)

    correctBallPositionOnce :: Actor ac -> Actor ac -> Actor ac 
    correctBallPositionOnce a@(Actor c) a0@(Actor c0) =
      let
        dist = ballsDistance a0 a
        overlap = d - dist
        dx = (c0.x - c.x) / dist
        dy = (c0.y - c.y) / dist
      in
        Actor c0{ x = c0.x + dx * overlap, y = c0.y + dy * overlap }

moveBall :: Time -> BoxWidth -> List GameActor -> GameActor -> GameActor
moveBall dt width balls actor@(Actor a) =
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
            
            -- check that ball reachs the top
            newBall1 = if vx /= newVx then ball{flying = Just {vx : newVx, vy : vy}} else ball
            newBall2 = if newY <= 0.0 then newBall1{flying = Nothing} else newBall1
            
            -- checks balls intersection
            intersectsBallsList = ballsIntersection 73.0 actor balls --TODO update 73.0
            actor2 = if null intersectsBallsList
              then Actor a { x = newX , y = newY, data = ActorBall newBall2}
              else
                let actor1 = Actor a { x = newX , y = newY, data = ActorBall newBall2{flying = Nothing}}
                 in correctBallPosition 73.0 actor1 intersectsBallsList
        in actor2
  _ -> actor

moveGun :: Time -> Array ControlKey -> GameActor -> GameActor
moveGun dt controlKeys actor@(Actor a) =
  case a.data of
    ActorGun gun ->
      let newSpeed = 
            let leftPressed = C.ArrowLeft `elem` controlKeys
                rightPressed = C.ArrowRight `elem` controlKeys
            in case Tuple leftPressed rightPressed of
                Tuple true false -> -gun.maxAngleSpeed
                Tuple false true -> gun.maxAngleSpeed
                _ -> 0.0
          newAngle' = a.angle + newSpeed * dt
          newAngle = clamp gun.maxLeftAngle gun.maxRightAngle newAngle'
      in Actor a{angle = newAngle, data = ActorGun gun{angleSpeed = newSpeed}}
    _ -> actor

fireBall :: AppGame Unit
fireBall = do
  model <- get
  let game = getGameRec model
  let (Actor gun) = game.actors.gun 
  let gunAngle = gun.angle
  let phi = pi * (90.0 - gunAngle)/180.0 
  let cosPhi = cos phi
  let sinPhi = sin phi
  let vx = cosPhi * game.ballSpeed
  let vy = - sinPhi * game.ballSpeed
  let ball = game.ballQueue{flying = Just {vx : vx, vy : vy}}

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
          act {
              recentlyAddedActors = (Tuple nameId "ActorBall") : mr.act.recentlyAddedActors 
              },
          game = GameState game{
            actors{balls = M.insert nameId newBallActor game.actors.balls},
            ballQueue = newQueueBall            
            }
        }


moveDragon :: Time -> Dragon -> GameActor -> GameActor
moveDragon dt dragon actor = actor

moveBallQueue :: Time -> BallQueueActor -> GameActor -> GameActor
moveBallQueue dt queue actor = actor

-- gameStep :: GameConfig -> Time -> AppMod GameConfig GameState Unit
gameStep :: GameStepFunc ActorData GameState
gameStep conf dt = do
  model <- get
  let m = getModelRec model
  let game = getGameRec model
  let controlKeys = mapMaybe read m.io.userInput.keys :: Array ControlKey
      prevControlKeys = mapMaybe read m.io.prevUserInput.keys :: Array ControlKey
      balls = flip List.filter (M.values game.actors.balls) $ \(Actor a) -> case a.data of 
                  ActorBall b | isNothing b.flying -> true
                  _ -> false
      updatedBalls = map (moveBall dt game.canvasWidth balls) game.actors.balls
      updatedGun = moveGun dt controlKeys game.actors.gun
  modmod $ \mr -> mr { game = GameState game { actors { balls = updatedBalls , gun = updatedGun }}}
  when (keyWasPressedOnce controlKeys prevControlKeys C.Space) fireBall
  let wsOut = m.io.wsIn --[]

  modmod $ \mr ->  mr { sys { gameStepNumber = mr.sys.gameStepNumber + 1}, io{wsOut = wsOut} }