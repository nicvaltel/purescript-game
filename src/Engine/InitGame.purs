module Engine.InitGame
  ( initGame
  ) where

import Prelude

import Data.Map (Map)
import Effect.Class (liftEffect)
import Engine.Config (Config)
import Engine.Model (Actor, AppModAff, Model, NameId, initialModelZeroTime, mkActorsFromConfig, modmodAff_, putModelAff)

initGame :: forall ac gm. 
  Config ac gm -> 
  gm -> 
  (gm -> ac -> ac) ->
  AppModAff ac gm Unit
initGame conf initialGameState mkActorData = do
  putModelAff (initialModelZeroTime initialGameState :: Model ac gm)
  actors :: Map NameId (Actor ac) <- liftEffect $ mkActorsFromConfig conf mkActorData
  modmodAff_ (\m -> m{ actors = actors } )
