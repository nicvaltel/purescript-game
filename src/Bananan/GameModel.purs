module Bananan.GameModel
  ( ConfigState
  , GameConfig
  , GameModel
  , GameState
  , GameActor
  )
  where

import Bananan.Reexport

import Bananan.Actors (ActorData)
import Bananan.Control (ControlKey)
import Engine.Config (Config)
import Engine.Model (Model, Actor)

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
type GameConfig = Config ActorData GameState
type GameActor = Actor ActorData