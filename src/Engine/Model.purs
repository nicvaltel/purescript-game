module Engine.Model
  ( Actor
  , Model(..)
  , initialModel
  , initialModelZeroTime
  , showModel
  ) where

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
import Engine.UserInput (UserInput, emptyUserInput)
import Graphics.Canvas (CanvasImageSource)
import Partial.Unsafe (unsafePartial)

type Actor ac
  = { nameId :: String
    , x :: Number
    , y :: Number
    , state :: ac
    }

type Model gm ac ui
  = { gameStepNumber :: Int
    , screenWidth :: Number
    , screenHeight :: Number
    , sprites :: Map String CanvasImageSource
    , lastUpdateTime :: Instant
    , actors :: Array (Actor ac)
    , gameState :: gm
    , prevUserInput :: UserInput ui
    }

showModel :: forall gm ac ui. Show gm => Show ac => Model gm ac ui -> String
showModel m =
  foldr (\str acc -> acc <> "\t" <> str <> "\n") "MODEL:\n"
    $ [ "gameStepNumber " <> show m.gameStepNumber
      -- , "gameTime " <> show m.gameTime
      , "screenWidth " <> show m.screenWidth
      , "screenHeight " <> show m.screenHeight
      , "lastUpdateTime " <> show m.lastUpdateTime
      , "actors " <> show m.actors
      , "gameState" <> show (m.gameState)
      ]

initialModel :: forall gm ac ui. Instant -> gm -> Model gm ac ui
initialModel currentTime gameState = (initialModelZeroTime gameState) { lastUpdateTime = currentTime }

initialModelZeroTime :: forall gm ac ui. gm -> Model gm ac ui
initialModelZeroTime gameState =
  unsafePartial
    $ let
        Just time = instant (Milliseconds 0.0)
      in
        { gameStepNumber: 0
        -- , gameTime: 0.0
        , screenWidth: 150.0
        , screenHeight: 100.0
        , sprites: Map.empty
        , lastUpdateTime: time
        , actors: []
        , gameState
        , prevUserInput : emptyUserInput
        }
