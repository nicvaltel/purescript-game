module Bananan.GameModel
  ( AppGame
  , GameActor
  , GameConfig
  , GameModel
  , GameState
  , mkActorData
  )
  where

-- import Bananan.Reexport

import Bananan.Actors (ActorData, Ball)
import Engine.Config (Config)
import Engine.Model (Actor, AppMod, Model, NameId)

type GameState = {
      score :: Int
    , ballQueue :: Ball
    , canvasWidth :: Number
    , gunNameId :: NameId
    , ballSpeed :: Number
  }

type GameModel = Model ActorData GameState
type GameConfig = Config ActorData GameState
type GameActor = Actor ActorData

type AppGame a = AppMod ActorData GameState a

mkActorData :: GameState -> ActorData -> ActorData
mkActorData _ actorData = actorData