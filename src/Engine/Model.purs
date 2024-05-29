module Engine.Model
  ( Actor
  , Model(..)
  , initialModel
  , initialModelZeroTime
  , showModel
  )
  where

import Prelude

import Data.DateTime.Instant (Instant, instant)
import Data.Foldable (foldr)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Time (Millisecond)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Now (now)
import Graphics.Canvas (CanvasImageSource)
import Partial.Unsafe (unsafePartial)

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
initialModel currentTime = initialModelZeroTime{lastUpdateTime = currentTime}


initialModelZeroTime :: Model
initialModelZeroTime = unsafePartial $
  let Just time = instant (Milliseconds 0.0)
  in 
    { gameStepNumber: 0
    -- , gameTime: 0.0
    , screenWidth: 150.0
    , screenHeight: 100.0
    , sprites: Map.empty
    , lastUpdateTime: time
    , actors: []
    }
