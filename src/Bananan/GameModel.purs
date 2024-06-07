module Bananan.GameModel
  ( ConfigState
  , GameActor
  , GameConfig
  , GameModel
  , GameState
  )
  where

import Bananan.Actors (TestBall)
import Bananan.Control (ControlKey)
import Engine.Config (Config)
import Engine.Model (class Actor, Model)

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

type GameModel = Model GameState TestBall ControlKey
type GameActor = TestBall
type GameConfig = Config Int String

