module Bananan.GameStep
  ( addRandomBalls
  , gameStep
  )
  where

import Bananan.Reexport
import Prelude

import Bananan.Actors (ActorData(..), BallQueueActor, Dragon, colorFromRandomInt, cssClassOfColor)
import Bananan.Control (ControlKey)
import Bananan.Control as C
import Bananan.GameConfig (GameConfig)
import Bananan.GameModel (AppGame, GameState(..), GameActor, getGameRec, modgs)
import Data.Foldable (for_)
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
type BoxHeight = Number
type Diameter = Number
type Y = Number


addRandomBalls :: Int -> Diameter -> BoxWidth -> Y -> AppGame Unit
addRandomBalls n ballDiameter width y = do
  let xOffset = (width - (toNumber n) * ballDiameter) / 2.0
  for_ (range 0 (n - 1)) $ \i -> do
    nameId <- mkNewNameId
    randN :: Int <- getRandom
    let color = colorFromRandomInt randN
    let newBallActor = Actor
          {
            nameId : nameId
          , x : xOffset + (toNumber i * ballDiameter)
          , y : y
          , width : ballDiameter
          , height : ballDiameter
          , z : 1
          , visible : true
          , angle : 0.0
          , cssClass : cssClassOfColor color
          , imageSource : ""
          , htmlElement : Nothing
          , data : ActorBall 
              { color : color
              , flying : Nothing
              } 
          }
    modmod $ \mr -> mr{ act { recentlyAddedActors = (Tuple nameId "ActorBall") : mr.act.recentlyAddedActors }}
    modgs $ \gs -> gs{ actors{balls = M.insert nameId newBallActor gs.actors.balls}}


findChainOfColor :: Diameter -> GameActor -> List GameActor -> List GameActor
findChainOfColor d actorBall@(Actor ball) staticBalls =
  case ball.data of
    ActorBall ab ->
      let balls = flip List.filter staticBalls $ \(Actor b) -> 
            case b.data of
              ActorBall aball -> ab.color == aball.color
              _ -> false
         
       in findChain actorBall balls List.Nil
    _ -> error $ "ERROR: findChainOfColor gets not a ball as agrument:" <> show actorBall

  where
    findChain :: GameActor -> List GameActor -> List GameActor -> List GameActor 
    findChain currentBall balls ballsInChain =
      let closeBalls = ballsIntersection (d * 1.05) currentBall balls
       in if List.null closeBalls
        then ballsInChain
        else
          let chain =
                flip List.concatMap closeBalls $ \cb ->
                  let newBallsInChain = List.Cons cb ballsInChain
                      newBallsInChainNames = map (\(Actor b) -> b.nameId) newBallsInChain
                      newBalls = List.filter (\(Actor b) -> b.nameId `List.notElem` newBallsInChainNames  ) balls
                  in findChain cb newBalls newBallsInChain
              nubChain = List.nubByEq (\(Actor x) (Actor y) -> x.nameId == y.nameId) chain -- TODO How to get rid of nub?
           in List.Cons currentBall nubChain

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

moveBall :: Time -> Diameter -> BoxWidth -> List GameActor -> GameActor -> GameActor
moveBall dt ballDiameter width balls actor@(Actor a) =
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
              intersectsBallsList = ballsIntersection ballDiameter actor balls
              actorResult = if null intersectsBallsList
                then Actor a { x = newX , y = newY, data = ActorBall newBall2}
                else
                  let actor1 = Actor a { x = newX , y = newY, data = ActorBall newBall2{flying = Nothing}}
                      actor2 = correctBallPosition ballDiameter actor1 intersectsBallsList
                      -- chain = findChainOfColor ballDiameter actor2 balls
                      -- tmpDeleteThis = trace ("findChainOfColor: " <> show chain ) $ \_ -> 1
                   in actor2
          in actorResult
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

fireBall :: Diameter -> AppGame Unit
fireBall ballDiameter = do
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

  let gunBottomX = gun.x + gun.width/2.0
  let gunBottomY = gun.y + gun.height
  let gunLenX = gun.height * cosPhi
  let gunLenY = gun.height * sinPhi
  let gunBarrelX = gunBottomX + gunLenX
  let gunBarrelY = gunBottomY - gunLenY

  nameId <- mkNewNameId
  randN :: Int <- getRandom
  let newQueueBall = {
          color : colorFromRandomInt randN
        , flying : Nothing
        }
  let newBallActor = Actor
        {
          nameId : nameId
        , x : gunBarrelX - (ballDiameter/2.0)
        , y : gunBarrelY - (ballDiameter/2.0)
        , width : ballDiameter
        , height : ballDiameter
        , z : 1
        , visible : true
        , angle : 0.0
        , cssClass : cssClassOfColor ball.color
        , imageSource : ""
        , htmlElement : Nothing
        , data : ActorBall ball
        }
  modmod $ \mr -> mr{ act { recentlyAddedActors = (Tuple nameId "ActorBall") : mr.act.recentlyAddedActors }}
  modgs $ \gs -> gs { actors{balls = M.insert nameId newBallActor game.actors.balls}, ballQueue = newQueueBall }


moveDragon :: Time -> Dragon -> GameActor -> GameActor
moveDragon dt dragon actor = actor

moveBallQueue :: Time -> BallQueueActor -> GameActor -> GameActor
moveBallQueue dt queue actor = actor

loseCondition :: Number -> Diameter -> BoxHeight -> List (Actor ActorData) -> Boolean
loseCondition loseHeightLevel ballDiameter boxHeight  balls =
  let yLevel = boxHeight - ballDiameter - loseHeightLevel
  in any (\(Actor ball) -> ball.y > yLevel ) balls

-- gameStep :: Config -> Time -> AppMod GameConfig GameState Unit
gameStep :: GameConfig -> GameStepFunc ActorData GameState
gameStep gameConf conf dt = do
  m <- getModelRec <$> get
  game <- getGameRec <$> get

  if not game.gameIsRunning 
    then pure unit
    else do
      let controlKeys = mapMaybe read m.io.userInput.keys :: Array ControlKey
          prevControlKeys = mapMaybe read m.io.prevUserInput.keys :: Array ControlKey
          staticBalls = flip List.filter (M.values game.actors.balls) $ \(Actor a) -> case a.data of 
                      ActorBall b | isNothing b.flying -> true
                      _ -> false
          updatedBalls = map (moveBall dt gameConf.ballDiameter game.canvasWidth staticBalls) game.actors.balls
          updatedGun = moveGun dt controlKeys game.actors.gun
      let isRunning = not (loseCondition gameConf.loseHeightLevel gameConf.ballDiameter game.canvasHeight staticBalls)
      modgs $ \gs -> gs { actors { balls = updatedBalls , gun = updatedGun }, gameIsRunning = isRunning}
      when (keyWasPressedOnce controlKeys prevControlKeys C.Space) (fireBall gameConf.ballDiameter)
      let wsOut = m.io.wsIn --[]

      modmod $ \mr ->  mr { sys { gameStepNumber = mr.sys.gameStepNumber + 1}, io{wsOut = wsOut} }