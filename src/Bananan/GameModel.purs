module Bananan.GameModel
  ( AppGame
  , GameActor
  , GameModel(..)
  , GameState(..)
  , GameStateRec
  , getGameRec
  , mkActorData
  , modgs
  )
  where

import Bananan.Reexport hiding ((:))

import Bananan.Actors (ActorData, Ball)
import Data.List ((:))
import Data.Map as M
import Engine.Model (class ActorContainer, Actor, AppMod, Model, NameId, checkActorNameId, getModelRec, modmod)

type GameStateRec = {
      score :: Int
    , ballQueue :: Ball
    , canvasWidth :: Number
    , canvasHeight :: Number
    , ballSpeed :: Number
    , gameIsRunning :: Boolean
    , actors :: 
        { balls :: M.Map NameId (Actor ActorData)
        , gun :: Actor ActorData
        , dragon :: Actor ActorData
        , ballQueueActor :: Actor ActorData
        }
  }

newtype GameState = GameState GameStateRec

instance showGameState :: Show GameState where
  show (GameState g) = show g

derive instance newtypeGameState :: Newtype GameState _

modgs :: (GameStateRec -> GameStateRec) -> AppGame Unit
modgs f = modmod $ \mr -> let (GameState g) = mr.game in mr { game = GameState (f g) }

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