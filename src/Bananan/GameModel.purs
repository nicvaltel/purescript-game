module Bananan.GameModel
  ( AppGame
  , GameActor
  , GameModel(..)
  , GameState(..)
  , GameStateRec
  , getGameRec
  , mkActorData
  )
  where

import Bananan.Reexport hiding ((:))

import Bananan.Actors (ActorData(..), Ball, BallColor(..), ballQueueActorMock, dragonMock, gunMock)
import Data.List ((:))
import Data.List (List(..))
import Data.Map as M
import Engine.Config (Config)
import Engine.Model (class ActorContainer, Actor(..), AppMod, Model, NameId, actorMock, checkActorNameId, getModelRec, lookupActor, mkUniqueNameId, modmod)

type GameStateRec = {
      score :: Int
    , ballQueue :: Ball
    , canvasWidth :: Number
    , gunNameId :: NameId
    , ballSpeed :: Number
    , actors :: 
        { balls :: M.Map NameId (Actor ActorData)
        , gun :: Actor ActorData
        , dragon :: Actor ActorData
        , ballQueueActor :: Actor ActorData
        }
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
        , actors : 
          { balls : M.empty
          , gun : let (Actor a) = actorMock in Actor a{data = ActorGun gunMock}
          , dragon : let (Actor a) = actorMock in Actor a{data = ActorDragon dragonMock}
          , ballQueueActor : let (Actor a) = actorMock in Actor a{data = ActorBallQueue ballQueueActorMock}
          }
        }
      _      -> Left $ TypeMismatch $ "Unknown GameState type: " <> gameStateType

instance showGameState :: Show GameState where
  show (GameState g) = show g

getGameRec :: GameModel -> GameStateRec
getGameRec m = let (GameState r) = (getModelRec m).game in r

type GameModel = Model ActorData GameState
type GameActor = Actor ActorData

type AppGame a = AppMod ActorData GameState a

mkActorData :: GameState -> ActorData -> ActorData
mkActorData _ actorData = actorData

instance actorContainerGameState :: ActorContainer ActorData GameState where
  getAllActors model = let as = (getGameRec model).actors in  
    as.ballQueueActor : as.dragon : as.gun : (M.values as.balls)

  updateActor nameId mbTypeName f = do
    g <- getGameRec <$> get
    case mbTypeName of
      Just "ActorGun" -> updateActorGun g
      Just "ActorBall" -> updateActorBall g
      Just "ActorDragon" -> updateActorDragon g
      Just "ActorBallQueue" -> undeteActorBallQueue g
      _ -> updateActorAnyType g
    where
      updateActorGun g = do
        modmod $ \mr -> mr{ game = GameState g {actors {gun = f g.actors.gun} }}
      updateActorBall g = do
        let newBalls = M.update (\a -> Just $ f a) nameId g.actors.balls
        modmod $ \mr -> mr{ game = GameState g {actors {balls = newBalls}}}
      updateActorDragon g = do
        modmod $ \mr -> mr{ game = GameState g {actors {dragon = f g.actors.dragon} }}
      undeteActorBallQueue g = do
        modmod $ \mr -> mr{ game = GameState g {actors {ballQueueActor = f g.actors.ballQueueActor} }}
      updateActorAnyType g =
        if checkActorNameId nameId g.actors.gun
          then updateActorGun g
          else if checkActorNameId nameId g.actors.dragon 
            then updateActorDragon g
            else if checkActorNameId nameId g.actors.ballQueueActor
              then undeteActorBallQueue g
              else updateActorBall g

  lookupActor nameId mbTypeName model =
    let ga = (getGameRec model).actors
    in
      case mbTypeName of
        Just "ActorGun" -> lookupGun ga
        Just "ActorBall" -> lookupBall ga
        Just "ActorDragon" -> lookupDragon ga
        Just "ActorBallQueue" -> lookupBallQueue ga
        _ -> lookupActorAnyType ga
    where
      lookupGun ga       = if checkActorNameId nameId ga.gun then Just ga.gun else Nothing
      lookupBall ga      = M.lookup nameId ga.balls
      lookupDragon ga    = if checkActorNameId nameId ga.dragon then Just ga.dragon else Nothing
      lookupBallQueue ga = if checkActorNameId nameId ga.ballQueueActor then Just ga.ballQueueActor else Nothing 
      lookupActorAnyType ga = 
        if checkActorNameId nameId ga.gun
              then Just ga.gun
              else if checkActorNameId nameId ga.dragon 
                then Just ga.dragon
                else if checkActorNameId nameId ga.ballQueueActor
                  then Just ga.ballQueueActor
                  else M.lookup nameId ga.balls