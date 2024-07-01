module Bananan.WSClient where

import Bananan.Reexport

import Bananan.Actors (ActorData(..), BallColor(..))
import Bananan.GameModel (GameModel, getGameRec)
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Combinators ((:=), (~>))
import Data.Array (fromFoldable)
import Data.Map as M
import Data.Maybe (isNothing)
import Engine.Model (Actor(..), NameId)


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
type FlyingBallPosition = {col :: BallColor, startX :: Int, startY :: Int, vx :: Number, vy :: Number} 

type ModelDiff = {
      gameIsRunning :: Maybe Boolean
    , shotsCounter :: Maybe Int
    , actors :: 
        { balls :: Maybe (Array BallPosition)
        , flyingBall :: Maybe (Either Int FlyingBallPosition) -- Maybe (Maybe FlyingBallPosition) doesnt work - decodeJson(encodeJson $ Just Nothing) decodes to Nothing, not to Just Nothing. So using Either instead
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
        }
    }

    where 
        fromFlyingBall ::  {flyball :: Actor ActorData, vx :: Number, vy :: Number} -> FlyingBallPosition
        fromFlyingBall fb = (\{flyball,vx, vy} -> let bp = actorToBallPosition flyball
                                      in {col: bp.col, startX : bp.x, startY : bp.y, vx: vx, vy: vy}) 
                                   fb

        change :: forall a. Eq a => a -> a -> Maybe a
        change old new = if old == new then Nothing else Just new



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
      isNothing md.actors.flyingBall = false
    | otherwise = true
