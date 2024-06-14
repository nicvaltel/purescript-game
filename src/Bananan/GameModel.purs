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

import Bananan.Actors (ActorData, Ball, BallColor(..))
import Data.Map as M
import Engine.Config (Config)
import Engine.Model (class ActorContainer, Actor, AppMod, Model, NameId, getModelRec, mkUniqueNameId, modmod)


type GameStateRec = {
      score :: Int
    , ballQueue :: Ball
    , canvasWidth :: Number
    , gunNameId :: NameId
    , ballSpeed :: Number
    , actors :: M.Map NameId (Actor ActorData)
  }

newtype GameState = GameState GameStateRec

instance decodeJsonGameState :: DecodeJson GameState where
  decodeJson json = do
    obj <- decodeJson json -- attempts to decode the JSON value as an object.
    gameStateType <- obj .: "type" -- extracts the "type" field from the JSON object.
    case gameStateType of
      -- "gameState" -> GameState <$> (obj .: "data") -- This pattern matches the "type" field to determine which constructor to use (ActorBall or ActorGun).
      -- TODO fix it!
      "gameState" -> Right $ GameState {
      score: 0
        , ballQueue : 
            {
              color : Red
            , flying : Nothing
            } 
        , canvasWidth : 100.0
        , gunNameId : mkUniqueNameId "gun"
        , ballSpeed : 0.8
        , actors : M.empty
        }
      _      -> Left $ TypeMismatch $ "Unknown GameState type: " <> gameStateType

instance showGameState :: Show GameState where
  show (GameState g) = show g

getGameRec :: GameModel -> GameStateRec
getGameRec m = let (GameState r) = (getModelRec m).game in r

type GameModel = Model GameState
type GameConfig = Config ActorData GameState
type GameActor = Actor ActorData

type AppGame a = AppMod GameState a

mkActorData :: GameState -> ActorData -> ActorData
mkActorData _ actorData = actorData

instance actorContainerGameState :: ActorContainer ActorData GameState where
  getAllActors model = M.values (getGameRec model).actors

  updateActor nameId f = do
    g <- getGameRec <$> get
    let newActors = M.update (\a -> Just $ f a) nameId g.actors 
    modmod $ \mr -> mr{ game = GameState g {actors = newActors }}

  lookupActor nameId model =
    let g = getGameRec model
     in M.lookup nameId (g.actors) 