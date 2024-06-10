module Bananan.Actors
  ( ActorData(..)
  , Ball(..)
  , BallColor(..)
  , BallQueueActor(..)
  , Dragon(..)
  , Gun(..)
  )
  where

import Bananan.Reexport

type Gun = {
    angleSpeed :: Number
  , maxAngleSpeed :: Number
  , maxLeftAngle :: Number
  , maxRightAngle :: Number
}

data BallColor = Red | Green | Blue | Yellow

instance showBallColor :: Show BallColor where
  show Red  = "Red"
  show Green = "Green"
  show Blue = "Blue"
  show Yellow = "Yellow"


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
  ,flying :: Maybe {vx :: Number, vy :: Number}
}


type Dragon = {
  animation :: String
}


type BallQueueActor = {
  animation :: String
}



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
