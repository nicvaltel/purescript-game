module Engine.InitGame
  ( initGame
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Traversable (for)
import Effect (Effect)
import Engine.Config (Config)
import Engine.Model (Actor(..), Model, initialModelZeroTime)
import Engine.ResourceLoader (getHtmlElement)
import Engine.Utils.Utils (undefined)

-- mkActorsFromConfig :: forall cfgac ui gm ac. 
--   cfgac -> 
--   (cfgac -> ac) ->
--   Effect (Actor ac)
-- mkActorsFromConfig conf mkActor = do


mkActorsFromConfig :: forall cfgac cfgst ac. 
  Config cfgac cfgst -> 
  (cfgac -> ac) ->
  Effect (Array (Actor ac))
mkActorsFromConfig conf mkActorData = do
  for conf.actors
    $ \a -> do
        mbElem <- getHtmlElement a.nameId
        pure
          { nameId: a.nameId
          , x: a.x
          , y: a.y
          , z: a.z
          , visible : true
          , angle : 0.0
          , htmlElement: mbElem
          , data: mkActorData a.data
          }


-- mkActors :: forall cfgac cfgst  ac. Config cfgac cfgst -> Effect (Array (Actor ac))
-- mkActors conf = pure []


initGame :: forall cfgac cfgst ui gm ac. 
  Config cfgac cfgst -> 
  gm -> 
  (cfgac -> ac) ->
  Effect (Model gm ac ui)
initGame conf initialGameState mkActorData = do
  let m = initialModelZeroTime initialGameState :: Model gm ac ui
  actors :: Array (Actor ac) <- mkActorsFromConfig conf mkActorData
  pure m{ actors = actors }
