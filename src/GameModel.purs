module GameModel
  ( Model(..)
  , initGame
  , showModel
  )
  where

import Prelude

import Data.Array (concatMap)
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Aff (Aff)
import Graphics.Canvas (CanvasImageSource)
import Signal.Time (Time)
import Data.Foldable (foldr)

type SpriteName = String

type Model
  = { gameStepNumber :: Int
    , gameTime :: Time
    -- , inputKey :: Int
    -- , wsBuffer :: Array String
    , screenWidth :: Number
    , screenHeight :: Number
    , sprites :: Map String CanvasImageSource
    }
showModel :: Model -> String
showModel m = 
  foldr (\str acc -> acc <> "\t" <> str <> "\n") "MODEL:\n" $
    [
      "gameStepNumber " <> show m.gameStepNumber 
    , "gameTime " <> show m.gameTime 
    , "screenWidth " <> show m.screenWidth 
    , "screenHeight " <> show m.screenHeight 
  ]


-- type GraphicsModel = {
--   background :: 
-- }

initialModel :: Model
initialModel =
  { gameStepNumber: 0
  , gameTime: 0.0
  -- , inputKey: 0
  -- , wsBuffer: []
  , screenWidth: 150.0
  , screenHeight: 100.0
  , sprites : Map.empty
  }

initGame :: Aff Model
initGame = pure initialModel
