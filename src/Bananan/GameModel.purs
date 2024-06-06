module Bananan.GameModel
  ( ConfigState
  , GameActor
  , GameConfig
  , GameModel
  , GameState
  )
  where

import Bananan.Actors ( ActorState)
import Bananan.Control (ControlKey)
import Engine.Config (Config)
import Engine.Model (Actor, Model)

type GameState = {
  score :: Int
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

