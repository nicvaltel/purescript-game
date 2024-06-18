module Bananan.Actors
  ( ActorData(..)
  , Ball(..)
  , BallColor(..)
  , BallQueueActor(..)
  , Dragon(..)
  , Gun(..)
  , ballQueueActorMock
  , colorFromRandomInt
  , cssClassOfColor
  , dragonMock
  , gunMock
  )
  where

import Bananan.Reexport
import Data.String (toLower)

type Gun = {
    angleSpeed :: Number
  , maxAngleSpeed :: Number
  , maxLeftAngle :: Number
  , maxRightAngle :: Number
}

gunMock :: Gun 
gunMock = 
  {
    angleSpeed : 0.0
  , maxAngleSpeed : 0.0
  , maxLeftAngle : 0.0
  , maxRightAngle : 0.0
  }

data BallColor = Red | Green | Blue | Yellow | Purple

derive instance eqBallColor :: Eq BallColor

instance showBallColor :: Show BallColor where
  show Red  = "Red"
  show Green = "Green"
  show Blue = "Blue"
  show Yellow = "Yellow"
  show Purple = "Purple"

colorFromRandomInt :: Int -> BallColor
colorFromRandomInt n = case n `mod` 5 of
  0 -> Red
  1 -> Green
  2 -> Blue
  3 -> Yellow
  _ -> Purple 

cssClassOfColor :: BallColor -> String
cssClassOfColor color = (toLower $ show color) <> "_ball"

instance decodeJsonBallColor :: DecodeJson BallColor where
  decodeJson json = do
    string <- decodeJson json
    note (TypeMismatch "BallColor") (ballColorFromString string)
    where
      ballColorFromString :: String -> Maybe BallColor
      ballColorFromString str = case str of
        "Red" -> Just Red
        "Blue" -> Just Blue
        "Yellow" -> Just Yellow
        _ -> Nothing

type Ball = {
   color :: BallColor
}


type Dragon = {
  animation :: String
}

dragonMock :: Dragon
dragonMock = {animation : ""}

type BallQueueActor = {
  animation :: String
}

ballQueueActorMock = {animation : ""}

data ActorData = 
    ActorGun Gun
  | ActorBall Ball
  | ActorDragon Dragon
  | ActorBallQueue BallQueueActor 

instance showActorData :: Show ActorData where
  show (ActorBall ball)  = "ActorBall" <> show ball
  show (ActorGun gun) = "ActorGun" <> show gun
  show (ActorDragon dragon) = "ActorDragon" <> show dragon
  show (ActorBallQueue ballQueue) = "ActorBallQueue" <> show ballQueue

instance decodeJsonActorData :: DecodeJson ActorData where
  decodeJson json = do
    obj <- decodeJson json -- attempts to decode the JSON value as an object.
    actorType <- obj .: "type" -- extracts the "type" field from the JSON object.
    case actorType of
      "ball" -> ActorBall <$> (obj .: "data") -- This pattern matches the "type" field to determine which constructor to use (ActorBall or ActorGun).
      "gun"  -> ActorGun <$> (obj .: "data")
      "dragon"  -> ActorDragon <$> (obj .: "data")
      "ball_queue_actor"  -> ActorBallQueue <$> (obj .: "data")
      _      -> Left $ TypeMismatch $ "Unknown actor type: " <> actorType
