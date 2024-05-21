module GameModel (Model(..), initGame) where

import Prelude
import Effect (Effect)
import Signal.Time (Time)
import Data.Tuple (Tuple)

type Model
  = { gameStepNumber :: Int
    , gameTime :: Time
    , inputKey :: Int
    , wsBuffer :: Array String
    , screenWidth :: Number
    , screenHeight :: Number
    }

-- type GraphicsModel = {
--   background :: 
-- }

initialModel :: Model
initialModel =
  { gameStepNumber: 0
  , gameTime: 0.0
  , inputKey: 0
  , wsBuffer: []
  , screenWidth: 150.0
  , screenHeight: 100.0
  }

initGame :: Effect Model
initGame = pure initialModel
