module Bananan.WSClient where

import Bananan.Reexport

import Bananan.Actors (ActorData(..), BallColor(..))
import Bananan.GameModel (GameActor, GameModel, GameStateRec, getGameRec)
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Combinators ((:=), (~>))
import Data.Array (fromFoldable)
import Data.Map as M
import Data.Maybe (isNothing)
import Engine.Model (Actor(..), NameId, getActorData, getActorRec)


data RemoteMessage = ModelDiffMsg ModelDiff | GameOverMsg

instance showWSMessage :: Show RemoteMessage where
  show (ModelDiffMsg mDiff) = "ModelDiffMsg: " <> show mDiff
  show GameOverMsg = "GameOverMsg" 

-- https://www.dgendill.com/posts/2017-03-05-purescript-json.html
-- https://github.com/purescript-contrib/purescript-argonaut-codecs/blob/main/docs/README.md
-- see EncodeJson AppUser / DecodeJson AppUser  
instance encodeRemoteMessage :: EncodeJson RemoteMessage where
  encodeJson (ModelDiffMsg modelDiff) = 
    "type" := "ModelDiffMsg"
      ~> "data" := encodeJson modelDiff
      ~> jsonEmptyObject
  encodeJson GameOverMsg =
    "type" := "GameOverMsg"
      ~> jsonEmptyObject 

instance decodeJsonRemoteMessage :: DecodeJson RemoteMessage where
  decodeJson json = do
    obj <- decodeJson json
    (msgType :: String) <- obj .: "type"
    case msgType of
      "ModelDiffMsg" -> do
        modelDiffData :: ModelDiff <- obj .: "data"
        pure (ModelDiffMsg modelDiffData)

      "GameOverMsg" -> pure GameOverMsg
      other -> Left (TypeMismatch other)


type BallPosition = {col :: BallColor, x :: Int, y :: Int}
type GunPosition = {angleSpeed :: Number, angle :: Number}
type FlyingBallPosition = {col :: BallColor, startX :: Int, startY :: Int, vx :: Number, vy :: Number} 

type ModelDiff = {
      gameIsRunning :: Maybe Boolean
    , shotsCounter :: Maybe Int
    , actors :: 
        { balls :: Maybe (Array BallPosition)
        , flyingBall :: Maybe (Either Int FlyingBallPosition) -- Maybe (Maybe FlyingBallPosition) doesnt work - decodeJson(encodeJson $ Just Nothing) decodes to Nothing, not to Just Nothing. So using Either instead
        , gun :: Maybe GunPosition -- angle speed
        , ballQueue :: Maybe BallColor
        }
}

mkModelDiff :: GameModel -> GameModel -> ModelDiff
mkModelDiff model0 model1 = do
    let g0 = getGameRec model0
    let g1 = getGameRec model1
    { gameIsRunning : change g0.gameIsRunning g1.gameIsRunning
    , shotsCounter : change g0.shotsCounter g1.shotsCounter
    , actors : 
        { balls : change (ballsToBallPositions g0.actors.balls) (ballsToBallPositions g1.actors.balls) 
        , flyingBall : 
            case Tuple g0.actors.flyingBall g1.actors.flyingBall of
                Tuple Nothing (Just fb) -> Just (Right (fromFlyingBall fb))
                Tuple (Just _) Nothing -> Just (Left 0) -- no matter what number, only Just Left matters
                _ -> Nothing
        , gun : fromGun g0.actors.gun g1.actors.gun
        , ballQueue : 
            case Tuple (getActorData g0.actors.ballQueue) (getActorData g1.actors.ballQueue) of
              Tuple (ActorBallQueue q0) (ActorBallQueue q1) -> change q0.nextBallColor q1.nextBallColor
              _ -> Nothing
        }
    }

    where 
        fromFlyingBall ::  {flyball :: Actor ActorData, vx :: Number, vy :: Number} -> FlyingBallPosition
        fromFlyingBall fb = (\{flyball,vx, vy} -> let bp = actorToBallPosition flyball
                                      in {col: bp.col, startX : bp.x, startY : bp.y, vx: vx, vy: vy}) fb

        fromGun :: GameActor -> GameActor -> Maybe GunPosition
        fromGun actor0 actor1 =
          case Tuple (getActorData actor0) (getActorData actor1) of
            Tuple (ActorGun gun0) (ActorGun gun1) ->
              if gun0.angleSpeed == gun1.angleSpeed
                then Nothing
                else Just {angleSpeed : gun1.angleSpeed, angle : (getActorRec actor1).angle}
            _ -> Nothing 

        change :: forall a. Eq a => a -> a -> Maybe a
        change old new = if old == new then Nothing else Just new

mkModelDiffInitial :: GameStateRec -> ModelDiff
mkModelDiffInitial gs = do
    { gameIsRunning : Just true
    , shotsCounter : Just 0
    , actors : 
        { balls : Just (ballsToBallPositions gs.actors.balls)
        , flyingBall : Nothing
        , gun : Nothing
        , ballQueue :
            case (getActorData gs.actors.ballQueue) of
              ActorBallQueue q -> Just q.nextBallColor 
              _ -> Nothing
        }
    }

actorToBallPosition :: Actor ActorData -> BallPosition
actorToBallPosition (Actor a) = case a.data of
    ActorBall ball -> {col : ball.color, x : round a.x, y : round a.y}
    _ -> {col: Red, x: 0, y: 0}


ballsToBallPositions :: M.Map NameId (Actor ActorData) -> Array BallPosition
ballsToBallPositions = map actorToBallPosition <<< fromFoldable <<< M.values

modelDiffChanged :: ModelDiff -> Boolean
modelDiffChanged md
    | isNothing md.gameIsRunning && 
      isNothing md.shotsCounter && 
      isNothing md.actors.balls &&
      isNothing md.actors.gun && 
      isNothing md.actors.ballQueue &&
      isNothing md.actors.flyingBall = false
    | otherwise = true
