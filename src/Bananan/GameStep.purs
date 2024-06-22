module Bananan.GameStep
  ( addRandomBalls
  , ballsIntersection
  , gameStep
  )
  where

import Bananan.Reexport

import Bananan.Actors (ActorData(..), BallQueueActor, Dragon, colorFromRandomInt, cssClassOfColor)
import Bananan.BallsGraph (addNodeBall, deleteNodeBall, findNotAttachedToCeilingBalls)
import Bananan.Control (ControlKey)
import Bananan.Control as C
import Bananan.GameConfig (GameConfig, selectBallQueueImageSource)
import Bananan.GameModel (AppGame, GameActor, GameState, getGameRec, getGameRec', modgs)
import Data.Array (fromFoldable)
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


addRandomBalls :: GameConfig -> Int -> BoxWidth -> Y -> AppGame Unit
addRandomBalls gameConf n width y = do
  let xOffset = (width - (toNumber n) * gameConf.ballDiameter) / 2.0
  let nearRange = gameConf.ballDiameter * gameConf.nearestBallDiameterFactor
  let 
    findNearesBalls ::  GameActor -> List GameActor -> List GameActor
    findNearesBalls ball allBalls = ballsIntersection nearRange ball allBalls


  for_ (range 0 (n - 1)) $ \i -> do
    nameId <- mkNewNameId
    randN :: Int <- getRandom
    let color = colorFromRandomInt randN
    let newBallActor = Actor
          {
            nameId : nameId
          , x : xOffset + (toNumber i * gameConf.ballDiameter)
          , y : y
          , width : gameConf.ballDiameter
          , height : gameConf.ballDiameter
          , z : 1
          , visible : true
          , angle : 0.0
          , cssClass : cssClassOfColor color
          , imageSource : selectBallQueueImageSource gameConf color
          , htmlElement : Nothing
          , data : ActorBall { color : color } 
          }
    game <- getGameRec <$> get
    let neighbours = findNearesBalls newBallActor (M.values game.actors.balls)
    let newGraphBall = addNodeBall newBallActor neighbours game.graphBall
    modmod $ \mr -> mr{ act { recentlyAddedActors = (Tuple nameId "ActorBall") : mr.act.recentlyAddedActors }}
    modgs $ \gs -> gs{graphBall = newGraphBall}
    modgs $ \gs -> gs{ actors{balls = M.insert nameId newBallActor gs.actors.balls}}


findChainOfColor :: Diameter -> Number -> GameActor -> List GameActor -> List GameActor
findChainOfColor d dFactor actorBall@(Actor ball) staticBalls =
  case ball.data of
    ActorBall ab ->
      let balls = flip List.filter staticBalls $ \(Actor b) -> 
            case b.data of
              ActorBall aball -> ab.color == aball.color && ball.nameId /= b.nameId
              _ -> false
         
       in findChain actorBall balls List.Nil
    _ -> error $ "ERROR: findChainOfColor gets not a ball as agrument:" <> show actorBall

  where
    findChain :: GameActor -> List GameActor -> List GameActor -> List GameActor 
    findChain currentBall balls ballsInChain =
      let closeBalls = ballsIntersection (d * dFactor) currentBall balls
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

moveFlyingBall :: Time -> Diameter -> Number -> BoxWidth -> Int -> AppGame Unit
moveFlyingBall dt ballDiameter dFactor width numberOfBallsInChainToDelete = do
  let nearRange = ballDiameter * dFactor
  let 
    findNearesBalls ::  GameActor -> List GameActor -> List GameActor
    findNearesBalls ball allBalls = ballsIntersection nearRange ball allBalls

  game <- getGameRec <$> get

  case game.actors.flyingBall of
    Just {flyball, vx,vy} -> do
        let (Actor fb) = flyball
        let newVx
              | fb.x <= 0.0 = abs vx
              | fb.x + fb.width >= width = -(abs vx)
              | otherwise = vx
        let newX = fb.x + dt * newVx
        let newY = max 0.0 (fb.y + dt * vy)
        let newFlyball = Actor fb{x = newX, y = newY}

        -- check that ball reachs the top
        if newY > 0.0
          then modgs $ \gs -> gs{actors{flyingBall = Just {flyball : newFlyball, vx : newVx, vy : vy}}}
          else do modgs $ \gs -> gs{actors{
                                    flyingBall = Nothing, 
                                    balls = M.insert fb.nameId newFlyball gs.actors.balls}}

        -- checks balls intersection
        let intersectsBallsList = ballsIntersection ballDiameter newFlyball (M.values game.actors.balls)
        when (not $ null intersectsBallsList) $ do
          let newFlyballCorrected = correctBallPosition ballDiameter newFlyball intersectsBallsList

          modgs $ \gs -> gs{actors{
                                    flyingBall = Nothing, 
                                    balls = M.insert fb.nameId newFlyballCorrected gs.actors.balls}}

        
        -- if flying ball just stopped right now, then process chain of balls of the same color
        gameUpdated <- getGameRec <$> get
        case gameUpdated.actors.flyingBall of
          Just _ -> pure unit
          Nothing -> unsafePartial $ do
            let (Just newStaticBall) =  M.lookup fb.nameId gameUpdated.actors.balls

            -- add newBall to graphBall
            let neighbours = findNearesBalls newStaticBall (M.values gameUpdated.actors.balls)
            let newGraphBall = addNodeBall newStaticBall neighbours gameUpdated.graphBall
            modgs $ \gs -> gs{graphBall = newGraphBall}

            -- find chain of balls of same color
            let chainToDelete = findChainOfColor ballDiameter dFactor newStaticBall (M.values gameUpdated.actors.balls)
            when (List.length chainToDelete >= numberOfBallsInChainToDelete) $ do
              let newBalls = foldr (\(Actor a) ballsAcc -> M.delete a.nameId ballsAcc) gameUpdated.actors.balls chainToDelete
              let deletedBalls = map (\(Actor a) -> a.nameId) chainToDelete
              modgs $ \gs -> gs{ actors{balls = newBalls}}
              modmod $ \mr -> mr{act{recentlyDeletedActors = mr.act.recentlyDeletedActors <> (fromFoldable deletedBalls) }}
              let newGrahpBall1 = foldr deleteNodeBall gameUpdated.graphBall (map (\(Actor a) -> a.nameId) chainToDelete)
              modgs $ \gs -> gs{graphBall = newGrahpBall1}

              -- find not attached balls
              gameUpdated2 <- getGameRec <$> get
              let unAttached = findNotAttachedToCeilingBalls gameUpdated2.graphBall
              let newBalls2 = foldr (\nameId ballsAcc -> M.delete nameId ballsAcc) gameUpdated2.actors.balls unAttached
              modgs $ \gs -> gs{ actors{balls = newBalls2}}
              modmod $ \mr -> mr{act{recentlyDeletedActors = mr.act.recentlyDeletedActors <> (fromFoldable unAttached) }}
              let newGrahpBall2 = foldr deleteNodeBall gameUpdated2.graphBall unAttached
              modgs $ \gs -> gs{graphBall = newGrahpBall2}

    _ -> pure unit


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

fireBall :: GameConfig -> Diameter -> AppGame Unit
fireBall gameConf ballDiameter = do
  model <- get
  let game = getGameRec model
  let (Actor gun) = game.actors.gun 
  let gunAngle = gun.angle
  let phi = pi * (90.0 - gunAngle)/180.0 
  let cosPhi = cos phi
  let sinPhi = sin phi
  let vx = cosPhi * game.ballSpeed
  let vy = - sinPhi * game.ballSpeed
  -- let ball = game.ballQueue

  let gunBottomX = gun.x + gun.width/2.0
  let gunBottomY = gun.y + gun.height
  let gunLenX = gun.height * cosPhi
  let gunLenY = gun.height * sinPhi
  let gunBarrelX = gunBottomX + gunLenX
  let gunBarrelY = gunBottomY - gunLenY

  nameId <- mkNewNameId
  randN :: Int <- getRandom
  let color = colorFromRandomInt randN
  let newQueueBall = { color : color }
  let newBallQAct = Actor (unwrap game.actors.ballQueueActor){imageSource = selectBallQueueImageSource gameConf color}
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
        , cssClass : cssClassOfColor game.ballQueue.color
        , imageSource : selectBallQueueImageSource gameConf game.ballQueue.color
        , htmlElement : Nothing
        , data : ActorBall game.ballQueue
        }
  modmod $ \mr -> mr{ act { recentlyAddedActors = (Tuple nameId "ActorBall") : mr.act.recentlyAddedActors }}
  modmod $ \mr -> mr {audioElemsToPlay = (getGameRec' mr).audio.shoot : mr.audioElemsToPlay}
  modgs $ \gs -> gs { actors { flyingBall = Just {flyball : newBallActor, vx, vy }
                             , ballQueueActor = newBallQAct}
                    , ballQueue = newQueueBall
                    , shotsCounter = gs.shotsCounter + 1 }


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
      moveFlyingBall dt gameConf.ballDiameter gameConf.nearestBallDiameterFactor game.canvasWidth gameConf.numberOfBallsInChainToDelete

      let controlKeys = mapMaybe read m.io.userInput.keys :: Array ControlKey
      let prevControlKeys = mapMaybe read m.io.prevUserInput.keys :: Array ControlKey
      let updatedGun = moveGun dt controlKeys game.actors.gun
      let isRunning = not (loseCondition gameConf.loseHeightLevel gameConf.ballDiameter game.canvasHeight (M.values game.actors.balls))
      
      modgs $ \gs -> gs { actors {gun = updatedGun }, gameIsRunning = isRunning}

      gameUpdated <- getGameRec <$> get
      when (isNothing gameUpdated.actors.flyingBall &&  keyWasPressedOnce controlKeys prevControlKeys C.Space) 
            (fireBall gameConf gameConf.ballDiameter)
      
      -- add new balls row
      let (Milliseconds deltaTime) = diff m.sys.lastUpdateTime game.lastRowsAdded.time
      when (deltaTime > gameConf.addNewRowsTimeInterval) $ do
        let deltaH = gameConf.ballDiameter * 0.866 -- 0.866 = sqrt(3)/2
        let nBalls = if game.lastRowsAdded.numberOfBalls == gameConf.ballsInSmallRow then gameConf.ballsInSmallRow + 1 else gameConf.ballsInSmallRow
        modgs $ \gs -> gs{actors {balls = map (\(Actor a) -> Actor a{y = a.y + deltaH}) gs.actors.balls}}
        addRandomBalls gameConf nBalls game.canvasWidth 0.0
        modgs $ \gs -> gs{lastRowsAdded = {time : m.sys.lastUpdateTime, numberOfBalls : nBalls}}

      let wsOut = m.io.wsIn --[]
      modmod $ \mr ->  mr { sys { gameStepNumber = mr.sys.gameStepNumber + 1}, io{wsOut = wsOut} }