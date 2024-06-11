module Bananan.GameModel
  ( AppGame
  , GameActor
  , GameConfig
  , GameModel
  , GameState
  , mkActorData
  )
  where

import Bananan.Reexport

import Bananan.Actors (ActorData, Ball)
import Engine.Config (Config)
import Engine.Model (Actor, Model, AppMod)

type GameState = {
      score :: Int
    , ballQueue :: Ball
  }

type GameModel = Model ActorData GameState
type GameConfig = Config ActorData GameState
type GameActor = Actor ActorData

type AppGame a = AppMod ActorData GameState a

mkActorData :: GameState -> ActorData -> ActorData
mkActorData gm actorData = actorData