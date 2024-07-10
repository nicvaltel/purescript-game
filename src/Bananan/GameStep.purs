module Bananan.GameStep
  ( addRandomBalls
  , ballsIntersection
  , gameStep
  )
  where

import Bananan.Reexport

import Bananan.Actors (ActorData(..), BallColor, BallQueue, Dragon, colorFromRandomInt, cssClassOfColor)
import Bananan.BallsGraph (addNodeBall, deleteNodeBall, findNotAttachedToCeilingBalls, updateAttachedToCeilingInGraphBall)
import Bananan.Control (ControlKey)
import Bananan.Control as C
import Bananan.GameConfig (GameConfig, selectBallQueueImageSource)
import Bananan.GameModel (AppGame, GameActor, GameState, getGameRec, getGameRec', modgs)
import Bananan.WSClient (BallPosition, FlyingBallPosition, ModelDiff, RemoteMessage(..), GunPosition, mkModelDiff, modelDiffChanged)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode (encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (fromFoldable)
import Data.Array as Array
import Data.Foldable (for_)
import Data.List (List)
import Data.List as List
import Data.Map as M
import Data.Maybe (isNothing, maybe)
import Data.Number (abs, cos, pi, sin, sqrt)
import Engine.GameLoop (GameStepFunc)
import Engine.Model (Actor(..), getActorData, getActorRec, getModelRec, getRandom, mkNewNameId, modActor, modmod)
import Engine.Types (Time)
import Engine.UserInput (keyWasPressedOnce)
import Engine.WebSocket.WSSignalChan as WS

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
    modmod $ \mr -> mr{ act { recentlyAddedActors = {nameId : nameId, parentElemId : gameConf.boards.boardElementId , clue : "ActorBall"} : mr.act.recentlyAddedActors }}
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


moveRemoteGun :: Time -> AppGame Unit
moveRemoteGun dt = do
  game <- getGameRec <$> get
  let gunActorRec = getActorRec game.remoteActors.gun 
  case gunActorRec.data of
    ActorGun gunData -> 
      let newAngle = clamp gunData.maxLeftAngle gunData.maxRightAngle (gunActorRec.angle + dt * gunData.angleSpeed )
          newGunActor = Actor gunActorRec{angle = newAngle}
      in modgs $ \ gs -> gs {remoteActors{gun = newGunActor}}
    _ -> pure unit

moveRemoteFlyingBall :: Time -> BoxWidth -> AppGame Unit
moveRemoteFlyingBall dt width = do
  game <- getGameRec <$> get
  case game.remoteActors.flyingBall of
    Just {flyball, vx,vy} -> do
        let (Actor fb) = flyball
        let newVx
              | fb.x <= 0.0 = abs vx
              | fb.x + fb.width >= width = -(abs vx)
              | otherwise = vx
        let newX = fb.x + dt * newVx
        let newY = max 0.0 (fb.y + dt * vy)
        let newFlyball = Actor fb{x = newX, y = newY}
        modgs $ \gs -> gs { remoteActors { flyingBall = Just {flyball : newFlyball, vx: newVx, vy }}}
    _ -> pure unit


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
  let nextRandcolor = colorFromRandomInt randN
  let flyingBallColor = unsafePartial $ let (ActorBallQueue nextBall) = getActorData game.actors.ballQueue in nextBall.nextBallColor
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
        , cssClass : cssClassOfColor flyingBallColor
        , imageSource : selectBallQueueImageSource gameConf flyingBallColor
        , htmlElement : Nothing
        , data : ActorBall {color : flyingBallColor}
        }
  modmod $ \mr -> mr{ act { recentlyAddedActors = {nameId : nameId, parentElemId : gameConf.boards.boardElementId , clue : "ActorBall"} : mr.act.recentlyAddedActors }}
  modmod $ \mr -> mr {audioElemsToPlay = (getGameRec' mr).audio.shoot : mr.audioElemsToPlay}
  modgs $ \gs -> gs { actors { flyingBall = Just {flyball : newBallActor, vx, vy },
                               ballQueue = Actor (getActorRec gs.actors.ballQueue) 
                                  { cssClass = cssClassOfColor nextRandcolor
                                  , imageSource = selectBallQueueImageSource gameConf nextRandcolor
                                  , data = ActorBallQueue {nextBallColor : nextRandcolor , animation : ""}
                                  } 
                              }
                    , shotsCounter = gs.shotsCounter + 1 }


moveDragon :: Time -> Dragon -> GameActor -> GameActor
moveDragon dt dragon actor = actor

moveBallQueue :: Time -> BallQueue -> GameActor -> GameActor
moveBallQueue dt queue actor = actor


processInputMessages :: GameConfig -> AppGame Unit
processInputMessages gameConf = do
  model <- get
  traverse_ processOneInputMessage (getModelRec model).io.wsIn

  where   
    processOneInputMessage :: WS.WSMessage -> AppGame Unit
    processOneInputMessage msg = do
      case (decodeJson <$> (jsonParser msg) :: Either String (Either JsonDecodeError RemoteMessage)) of
        Right (Right remMsg) -> case remMsg of
          ModelDiffMsg mDiff -> do
            processModelDiff mDiff
          _ -> pure unit
        _ -> pure unit

    processModelDiff :: ModelDiff -> AppGame Unit
    processModelDiff mDiff = do
      maybe (pure unit) updateRemoteBalls mDiff.actors.balls
      maybe (pure unit) updateRemoteFlyingBall mDiff.actors.flyingBall
      maybe (pure unit) updateRemoteGun mDiff.actors.gun
      maybe (pure unit) updateRemoteBallQueue mDiff.actors.ballQueue

    updateRemoteBallQueue :: BallColor -> AppGame Unit
    updateRemoteBallQueue color = modgs $ \gs -> gs{remoteActors 
      {ballQueue = modActor gs.remoteActors.ballQueue (\a -> 
        case a.data of
          ActorBallQueue q -> a{ 
              imageSource = selectBallQueueImageSource gameConf color
            , data = ActorBallQueue q{nextBallColor = color}
            }
          _ -> a
      )}}

    updateRemoteGun :: GunPosition -> AppGame Unit
    updateRemoteGun gunPos = modgs $ \gs -> gs{remoteActors 
      {gun = modActor gs.remoteActors.gun (\a -> 
        case a.data of
          ActorGun gun -> a{ data = ActorGun gun{angleSpeed = gunPos.angleSpeed}
                           , angle = gunPos.angle}
          _ -> a
      )}}
    updateRemoteBalls :: Array BallPosition -> AppGame Unit
    updateRemoteBalls newBallsPositions = do
      gsr <- getGameRec  <$> get
      let deletedBallsNamesId = map (\(Actor a) -> a.nameId) $ M.values gsr.remoteActors.balls
      modmod $ \mr -> mr{act{recentlyDeletedActors = mr.act.recentlyDeletedActors <> (fromFoldable deletedBallsNamesId) }}
      modgs $ \gs -> gs{remoteActors{balls = M.empty}}

      for_ newBallsPositions $ \bp -> do
        nameId <- mkNewNameId
        let newBallActor = Actor
              {
                nameId : nameId
              , x : toNumber bp.x
              , y : toNumber bp.y
              , width : gameConf.ballDiameter
              , height : gameConf.ballDiameter
              , z : 1
              , visible : true
              , angle : 0.0
              , cssClass : cssClassOfColor bp.col
              , imageSource : selectBallQueueImageSource gameConf bp.col
              , htmlElement : Nothing
              , data : ActorBall { color : bp.col } 
              }
        modmod $ \mr -> mr{ act { recentlyAddedActors = {nameId : nameId, parentElemId : gameConf.boards.remoteBoardElementId , clue : "RemoteActorBall"} : mr.act.recentlyAddedActors }}
        modgs $ \gs -> gs{ remoteActors{balls = M.insert nameId newBallActor gs.remoteActors.balls}}

    updateRemoteFlyingBall :: Either Int FlyingBallPosition -> AppGame Unit
    updateRemoteFlyingBall (Left _) = do
      gsr <- getGameRec  <$> get
      case gsr.remoteActors.flyingBall of
        Nothing -> pure unit
        Just {flyball} -> modmod $ \mr -> mr{act{recentlyDeletedActors = (getActorRec flyball).nameId : mr.act.recentlyDeletedActors }}
      modgs $ \gs -> gs{remoteActors{flyingBall = Nothing}}
    updateRemoteFlyingBall (Right fb) = do
      nameId <- mkNewNameId
      let newFlyBallActor = Actor
            {
              nameId : nameId
            , x : toNumber fb.startX
            , y : toNumber fb.startY
            , width : gameConf.ballDiameter
            , height : gameConf.ballDiameter
            , z : 1
            , visible : true
            , angle : 0.0
            , cssClass : cssClassOfColor fb.col
            , imageSource : selectBallQueueImageSource gameConf fb.col
            , htmlElement : Nothing
            , data : ActorBall { color : fb.col } 
            }
      modmod $ \mr -> mr{ act { recentlyAddedActors = {nameId : nameId, parentElemId : gameConf.boards.remoteBoardElementId , clue : "RemoteActorBall"} : mr.act.recentlyAddedActors }}
      modgs $ \gs -> gs{ remoteActors
        {flyingBall = Just{flyball : newFlyBallActor, vx : fb.vx, vy: fb.vy}}}

loseCondition :: Number -> Diameter -> BoxHeight -> List (Actor ActorData) -> Boolean
loseCondition loseHeightLevel ballDiameter boxHeight  balls =
  let yLevel = boxHeight - ballDiameter - loseHeightLevel
  in any (\(Actor ball) -> ball.y > yLevel ) balls

-- gameStep :: Config -> Time -> AppMod GameConfig GameState Unit
gameStep :: GameConfig -> GameStepFunc ActorData GameState
gameStep gameConf conf dt = do
  model0 <- get
  m <- getModelRec <$> get
  game <- getGameRec <$> get

  if not game.gameIsRunning 
    then pure unit
    else do
      processInputMessages gameConf

      moveFlyingBall dt gameConf.ballDiameter gameConf.nearestBallDiameterFactor game.boards.board.width gameConf.numberOfBallsInChainToDelete
      moveRemoteFlyingBall dt game.boards.board.width
      moveRemoteGun dt

      let controlKeys = mapMaybe read m.io.userInput.keys :: Array ControlKey
      let prevControlKeys = mapMaybe read m.io.prevUserInput.keys :: Array ControlKey
      let updatedGun = moveGun dt controlKeys game.actors.gun
      let isRunning = not (loseCondition gameConf.loseHeightLevel gameConf.ballDiameter game.boards.board.height (M.values game.actors.balls))
      
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
        addRandomBalls gameConf nBalls game.boards.board.width 0.0
        -- modgs $ \gs -> gs{graphBall = updateAttachedToCeilingInGraphBall gs.actors.balls gs.graphBall}
        modgs $ \gs -> gs{lastRowsAdded = {time : m.sys.lastUpdateTime, numberOfBalls : nBalls}}

      modelDiff <- (mkModelDiff model0) <$> get
      let wsOut = Array.catMaybes 
            [ if modelDiff.gameIsRunning == Just false then Just (stringify $ encodeJson GameOverMsg) else Nothing
            , if modelDiffChanged modelDiff then Just (stringify $ encodeJson (ModelDiffMsg modelDiff)) else Nothing  
            ]
      modmod $ \mr ->  mr { sys { gameStepNumber = mr.sys.gameStepNumber + 1}, io{wsOut = wsOut <> mr.io.wsOut} }