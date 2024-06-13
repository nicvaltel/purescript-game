module Bananan.GameModel
  ( AppGame
  , GameActor
  , GameConfig
  , GameModel(..)
  , GameState(..)
  , GameStateRec
  , getGameRec
  , mkActorData
  )
  where

import Bananan.Reexport

import Bananan.Actors (ActorData, Ball)
import Data.List (List)
import Data.Map as M
import Engine.Config (Config)
import Engine.Model (class ActorContainer, Actor, AppMod, Model, NameId, getAllActors, getModelRec, lookupActor, modmod)
import Engine.Utils.Utils (undefined)

type GameStateRec = {
      score :: Int
    , ballQueue :: Ball
    , canvasWidth :: Number
    , gunNameId :: NameId
    , ballSpeed :: Number
  }

newtype GameState = GameState GameStateRec

instance decodeJsonGameState :: DecodeJson GameState where
  decodeJson json = do
    obj <- decodeJson json -- attempts to decode the JSON value as an object.
    gameStateType <- obj .: "type" -- extracts the "type" field from the JSON object.
    case gameStateType of
      "gameState" -> GameState <$> (obj .: "data") -- This pattern matches the "type" field to determine which constructor to use (ActorBall or ActorGun).
      _      -> Left $ TypeMismatch $ "Unknown GameState type: " <> gameStateType

instance showGameState :: Show GameState where
  show (GameState g) = show g

getGameRec :: GameModel -> GameStateRec
getGameRec m = let (GameState r) = (getModelRec m).game in r

type GameModel = Model ActorData GameState
type GameConfig = Config ActorData GameState
type GameActor = Actor ActorData

type AppGame a = AppMod ActorData GameState a

mkActorData :: GameState -> ActorData -> ActorData
mkActorData _ actorData = actorData

instance actorContainerGameState :: ActorContainer ActorData GameState where
  getAllActors model = M.values (getModelRec model).act.actors

  updateActor nameId f = do
    m <- getModelRec <$> get
    case M.lookup nameId m.act.actors of
      Just _ -> 
        let newActors = M.update (\a -> Just $ f a) nameId m.act.actors 
          in modmod $ \mr -> mr{ act{actors = newActors }}
      Nothing -> pure unit

  lookupActor nameId model =
    let m = getModelRec model
     in M.lookup nameId (m.act.actors) 