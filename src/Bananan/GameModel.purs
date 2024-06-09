module Bananan.GameModel
  ( GameActor
  , GameConfig
  , GameModel
  , GameState
  , mkActorData
  )
  where

import Bananan.Reexport

import Bananan.Actors (ActorData, Ball)
import Engine.Config (Config)
import Engine.Model (Model, Actor)

type GameState = {
    score :: Int
    , ballQueue :: Ball
  }


type GameModel = Model ActorData GameState
type GameConfig = Config ActorData GameState
type GameActor = Actor ActorData

mkActorData :: GameState -> ActorData -> ActorData
mkActorData gm actorData = actorData