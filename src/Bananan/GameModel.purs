module Bananan.GameModel
  ( AppGame
  , Board
  , GameActor
  , GameModel(..)
  , GameState(..)
  , GameStateRec
  , getGameRec
  , getGameRec'
  , mkActorData
  , modgs
  )
  where

import Bananan.Reexport hiding ((:))
import Prelude

import Bananan.Actors (ActorData)
import Bananan.BallsGraph (GraphBall)
import Data.List (List(..), (:), catMaybes)
import Data.Map as M
import Engine.Model (class ActorContainer, Actor, AppMod, Model, NameId, ModelRec, checkActorNameId, getActorRec, getModelRec, modmod)
import Record as Record
import Web.HTML.HTMLMediaElement (HTMLMediaElement)

type Board = 
  { top    :: Number
  , left   :: Number
  , width  :: Number
  , height :: Number 
  }

type GameStateRec = {
      score :: Int
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
        , ballQueue :: Actor ActorData
        }
    , remoteActors :: { balls :: M.Map NameId (Actor ActorData)
        , flyingBall :: Maybe {flyball :: Actor ActorData, vx :: Number, vy :: Number}
        , gun :: Actor ActorData
        , dragon :: Actor ActorData
        , ballQueue :: Actor ActorData
        }
    , graphBall :: GraphBall
    , audio :: { shoot :: HTMLMediaElement }
    , boards :: {board :: Board, remoteBoard :: Board}
  }

newtype GameState = GameState GameStateRec

instance showGameState :: Show GameState where
  show (GameState g) = show $
    Record.delete (Proxy :: Proxy "audio") $ 
    Record.delete (Proxy :: Proxy "graphBall") g

derive instance newtypeGameState :: Newtype GameState _

modgs :: (GameStateRec -> GameStateRec) -> AppGame Unit
modgs f = modmod $ \mr -> let (GameState g) = mr.game in mr { game = GameState (f g) }

getGameRec :: GameModel -> GameStateRec
getGameRec m = let (GameState r) = (getModelRec m).game in r

getGameRec' :: ModelRec ActorData GameState -> GameStateRec
getGameRec' m = let (GameState r) = m.game in r

type GameModel = Model ActorData GameState
type GameActor = Actor ActorData

type AppGame a = AppMod ActorData GameState a

mkActorData :: GameState -> ActorData -> ActorData
mkActorData _ actorData = actorData


instance actorContainerGameState :: ActorContainer ActorData GameState where
  getAllActors model = 
    let as = (getGameRec model).actors
        ras = (getGameRec model).remoteActors
        staticActors = 
          as.ballQueue : 
          as.dragon : 
          as.gun : 
          ras.ballQueue : 
          ras.dragon : 
          ras.gun : 
          (M.values as.balls <> M.values ras.balls)
     in (map (\{flyball} -> flyball) $ catMaybes $ as.flyingBall : ras.flyingBall : Nil) <> staticActors

  updateActor nameId mbTypeName f = do
    g <- getGameRec <$> get
    case mbTypeName of
      Nothing -> updateActorAnyType g
      Just "ActorGun" -> updateActorGun 
      Just "ActorBall" -> updateActorBall 
      Just "ActorDragon" -> updateActorDragon 
      Just "ActorBallQueue" -> updeteActorBallQueue
      Just "RemoteActorGun" -> updateRemoteActorGun 
      Just "RemoteActorBall" -> updateRemoteActorBall 
      Just "RemoteActorDragon" -> updateRemoteActorDragon 
      Just "RemoteActorBallQueue" -> updeteRemoteActorBallQueue 
      _ -> updateActorAnyType g
    where
      updateActorGun = do
        modgs $ \gs -> gs {actors {gun = f gs.actors.gun} }
      updateRemoteActorGun = do
        modgs $ \gs -> gs {remoteActors {gun = f gs.remoteActors.gun} }
      updateActorBall = do
            modgs $ \gs ->
                case gs.actors.flyingBall of
                  Just {flyball, vx,vy} | (getActorRec flyball).nameId == nameId ->
                        gs {actors {flyingBall = Just {flyball : f flyball, vx, vy }}}
                  _ -> let newBalls = M.update (\a -> Just $ f a) nameId gs.actors.balls
                        in gs {actors{ balls = newBalls} }
      updateRemoteActorBall = do
            modgs $ \gs ->
                case gs.remoteActors.flyingBall of
                  Just {flyball, vx,vy} | (getActorRec flyball).nameId == nameId ->
                        gs {remoteActors {flyingBall = Just {flyball : f flyball, vx, vy }}}
                  _ -> let newBalls = M.update (\a -> Just $ f a) nameId gs.remoteActors.balls
                        in gs {remoteActors{ balls = newBalls} }
      updateActorDragon = do
        modgs $ \gs -> gs {actors {dragon = f gs.actors.dragon }}
      updateRemoteActorDragon = do
        modgs $ \gs -> gs {remoteActors {dragon = f gs.remoteActors.dragon }}
      updeteActorBallQueue = do
        modgs $ \gs -> gs {actors {ballQueue = f gs.actors.ballQueue }}
      updeteRemoteActorBallQueue = do
        modgs $ \gs -> gs {actors {ballQueue = f gs.remoteActors.ballQueue }}
      updateActorAnyType g
        | checkActorNameId nameId g.actors.gun = updateActorGun 
        | checkActorNameId nameId g.actors.dragon = updateActorDragon 
        | checkActorNameId nameId g.actors.ballQueue = updeteActorBallQueue
        | checkActorNameId nameId g.remoteActors.gun = updateRemoteActorGun 
        | checkActorNameId nameId g.remoteActors.dragon = updateRemoteActorDragon 
        | checkActorNameId nameId g.remoteActors.ballQueue = updeteRemoteActorBallQueue
        | otherwise = do
            updateActorBall
            updateRemoteActorBall 

  lookupActor nameId mbTypeName model =
    let ga = (getGameRec model).actors
        gar = (getGameRec model).remoteActors
    in
      case mbTypeName of
        Nothing -> lookupActorAnyType ga 
        Just "ActorGun" -> lookupGun ga
        Just "ActorBall" -> lookupBall ga
        Just "ActorDragon" -> lookupDragon ga
        Just "ActorBallQueue" -> lookupBallQueue ga
        Just "RemoteActorGun" -> lookupGun gar
        Just "RemoteActorBall" -> lookupBall gar
        Just "RemoteActorDragon" -> lookupDragon gar
        Just "RemoteActorBallQueue" -> lookupBallQueue gar
        _ -> lookupActorAnyType ga
    where
      lookupGun ga       = if checkActorNameId nameId ga.gun then Just ga.gun else Nothing
      lookupBall ga      = case ga.flyingBall of
                              Just {flyball} | (getActorRec flyball).nameId == nameId -> Just flyball
                              _ -> M.lookup nameId ga.balls
      lookupDragon ga    = if checkActorNameId nameId ga.dragon then Just ga.dragon else Nothing
      lookupBallQueue ga = if checkActorNameId nameId ga.ballQueue then Just ga.ballQueue else Nothing 
      lookupActorAnyType ga
        | checkActorNameId nameId ga.gun = Just ga.gun
        | checkActorNameId nameId ga.dragon = Just ga.dragon
        | checkActorNameId nameId ga.ballQueue = Just ga.ballQueue
        | otherwise = M.lookup nameId ga.balls
