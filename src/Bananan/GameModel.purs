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
import Record as Record

import Bananan.Actors (ActorData, Ball, BallColor)
import Bananan.BallsGraph (GraphBall)
import Data.List (List, (:))
import Data.Map as M
import Engine.Model (class ActorContainer, Actor(..), AppMod, Model, NameId, checkActorNameId, getActorRec, getModelRec, modmod)

type GameStateRec = {
      score :: Int
    , ballQueue :: Ball
    , canvasWidth :: Number
    , canvasHeight :: Number
    , ballSpeed :: Number
    , gameIsRunning :: Boolean
    , shotsCounter :: Int
    , lastRowsAdded :: { time :: Instant, numberOfBalls :: Int}
    , actors :: 
        { balls :: M.Map NameId (Actor ActorData)
        , flyingBall :: Maybe {flyball :: Actor ActorData, vx :: Number, vy :: Number}
        , gun :: Actor ActorData
        , dragon :: Actor ActorData
        , ballQueueActor :: Actor ActorData
        }
    , graphBall :: GraphBall
  }

newtype GameState = GameState GameStateRec

instance showGameState :: Show GameState where
  show (GameState g) = show $ Record.delete (Proxy :: Proxy "graphBall") g

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
  getAllActors model = 
    let as = (getGameRec model).actors  
        allActors = as.ballQueueActor : as.dragon : as.gun : (M.values as.balls)
     in case as.flyingBall of
      Just{flyball} -> flyball : allActors
      _ -> allActors

  updateActor nameId mbTypeName f = do
    g <- getGameRec <$> get
    case mbTypeName of
      Just "ActorGun" -> updateActorGun 
      Just "ActorBall" -> updateActorBall 
      Just "ActorDragon" -> updateActorDragon 
      Just "ActorBallQueue" -> undeteActorBallQueue 
      _ -> updateActorAnyType g
    where
      updateActorGun = do
        modgs $ \gs -> gs {actors {gun = f gs.actors.gun} }
      updateActorBall = do
            modgs $ \gs ->
                case gs.actors.flyingBall of
                  Just {flyball, vx,vy} | (getActorRec flyball).nameId == nameId ->
                        gs {actors {flyingBall = Just {flyball : f flyball, vx, vy }}}
                  _ -> let newBalls = M.update (\a -> Just $ f a) nameId gs.actors.balls
                        in gs {actors{ balls = newBalls} }
      updateActorDragon = do
        modgs $ \gs -> gs {actors {dragon = f gs.actors.dragon }}
      undeteActorBallQueue = do
        modgs $ \gs -> gs {actors {ballQueueActor = f gs.actors.ballQueueActor }}
      updateActorAnyType g =
        if checkActorNameId nameId g.actors.gun
          then updateActorGun 
          else if checkActorNameId nameId g.actors.dragon 
            then updateActorDragon 
            else if checkActorNameId nameId g.actors.ballQueueActor
              then undeteActorBallQueue 
              else updateActorBall 

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
      lookupBall ga      = case ga.flyingBall of
                              Just {flyball, vx,vy} | (getActorRec flyball).nameId == nameId -> Just flyball -- TODO how to remove vx, vy?
                              _ -> M.lookup nameId ga.balls
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
