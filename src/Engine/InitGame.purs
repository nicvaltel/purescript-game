module Engine.InitGame
  ( initGame
  ) where

import Prelude

import Data.Map (Map)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Engine.Config (Config)
import Engine.Model (Actor, AppModAff, Model, NameId, initialModelZeroTime, modmodAff, putModelAff)

initGame :: forall ac gm. 
  Config -> 
  gm -> 
  (gm -> ac -> ac) ->
  AppModAff ac gm Unit
initGame conf initialGameState mkActorData = do
  putModelAff (initialModelZeroTime initialGameState :: Model ac gm)
  -- TODO mkActorsFromConfig
  -- actors :: Map NameId (Actor ac) <- liftEffect $ mkActorsFromConfig conf mkActorData
  -- modmodAff (\m -> m{act{ actors = actors } } )
