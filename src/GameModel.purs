module GameModel
  ( Actor
  , Model(..)
  , initGame
  , showModel
  ) where

import Prelude
import Data.DateTime.Instant (Instant)
import Data.Foldable (foldr)
import Data.Map (Map)
import Data.Map as Map
import Effect (Effect)
import Graphics.Canvas (CanvasImageSource)
-- import Signal.Time (Time)
import Effect.Now (now)



type Actor
  = { name :: String
    , x :: Number
    , y :: Number
    , vx :: Number
    , vy :: Number
    , spriteName :: String
    }

type Model
  = { gameStepNumber :: Int
    -- , gameTime :: Time
    , screenWidth :: Number
    , screenHeight :: Number
    , sprites :: Map String CanvasImageSource
    , lastUpdateTime :: Instant
    , actors :: Array Actor
    }

showModel :: Model -> String
showModel m =
  foldr (\str acc -> acc <> "\t" <> str <> "\n") "MODEL:\n"
    $ [ "gameStepNumber " <> show m.gameStepNumber
      -- , "gameTime " <> show m.gameTime
      , "screenWidth " <> show m.screenWidth
      , "screenHeight " <> show m.screenHeight
      , "lastUpdateTime " <> show m.lastUpdateTime
      , "actors " <> show m.actors
      ]

initialModel :: Instant -> Model
initialModel currentTime =
  { gameStepNumber: 0
  -- , gameTime: 0.0
  , screenWidth: 150.0
  , screenHeight: 100.0
  , sprites: Map.empty
  , lastUpdateTime: currentTime
  , actors: []
  }

actorBall :: Actor
actorBall = 
  { name : "actor_red_ball"
    , x : 17.0
    , y : 22.0
    , vx : 31.0/100.0
    , vy : 23.0/100.0
    , spriteName : "red_ball"
    }
populateActors :: Model -> Model
populateActors m = m {actors = [actorBall]}

initGame :: Effect Model
initGame = do
  currentTime <- now
  pure $ populateActors $ initialModel currentTime
