module Bananan.Actors
  ( Ball(..)
  , BallColor(..)
  , BallQueue(..)
  , ActorState(..)
  , Dragon(..)
  , Gun(..)
  )
  where

import Bananan.Reexport

import Data.Argonaut.Decode ((.:))

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

type Gun = {
    angleSpeed :: Number
  , maxAngleSpeed :: Number
  , maxLeftAngle :: Number
  , maxRightAngle :: Number
}

type Dragon = {
  animation :: String
}

type Ball = {
   color :: BallColor
  ,flying :: Maybe {vx :: Number, vy :: Number}
}

type BallQueue = {
  queue :: Array Ball
}

data ActorState = 
    ActorBall Ball 
  | ActorGun Gun 
  | ActorDragon Dragon 
  | ActorBallQueue BallQueue


instance showActorState :: Show ActorState where
  show (ActorBall ball)  = "ActorBall" <> show ball
  show (ActorGun gun) = "ActorGun" <> show gun
  show (ActorDragon dragon) = "ActorDragon" <> show dragon
  show (ActorBallQueue ballQueue) = "ActorBallQueue" <> show ballQueue

instance decodeJsonActorState :: DecodeJson ActorState where
  decodeJson json = do
    obj <- decodeJson json -- attempts to decode the JSON value as an object.
    actorType <- obj .: "type" -- extracts the "type" field from the JSON object.
    case actorType of
      "ball" -> ActorBall <$> (obj .: "data") -- This pattern matches the "type" field to determine which constructor to use (ActorBall or ActorGun).
      "gun"  -> ActorGun <$> (obj .: "data")
      "dragon"  -> ActorDragon <$> (obj .: "data")
      "ball_queue"  -> ActorBallQueue <$> (obj .: "data")
      _      -> Left $ TypeMismatch $ "Unknown actor type: " <> actorType
