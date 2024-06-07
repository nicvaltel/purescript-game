module Bananan.GameModel
  ( ConfigState
  , GameConfig
  , GameModel
  , GameState
  )
  where

import Bananan.Actors (ActorData)
import Bananan.Control (ControlKey)
import Engine.Config (Config)
import Engine.Model (Model)
import Bananan.Reexport

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

type GameModel = Model GameState ActorData ControlKey
type GameConfig = Config Json Json

