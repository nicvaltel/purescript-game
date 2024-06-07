module Bananan.GameModel
  ( GameActor
  , GameConfig
  , GameModel
  , GameState
  , mkActorData
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


type GameModel = Model ActorData GameState ControlKey
type GameConfig = Config ActorData GameState
type GameActor = Actor ActorData

mkActorData :: GameState -> ActorData -> ActorData
mkActorData gm actorData = actorData