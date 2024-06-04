module GameModel
  ( ActorState
  , ConfigState
  , GameActor
  , GameConfig
  , GameModel
  , GameState
  )
  where

import Prelude
import Engine.Config (Config)
import Engine.Model (Actor, Model)
import Control(ControlKey)

type GameState = {
  gridSize :: Int
}
type ActorState = {
  vx :: Number,
  vy :: Number
}

type ConfigState = {
  gridNSize :: Int,
  boardPixelSize :: Int,
  boardBorderPixelSize :: Int,
  stoneSpriteSize :: Int,
  stoneScale :: Number
}

type GameModel = Model GameState ActorState ControlKey
type GameActor = Actor ActorState
type GameConfig = Config ConfigState ActorState

