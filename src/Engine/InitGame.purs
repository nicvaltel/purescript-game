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

-- mkActors :: forall cfg ac. Config cfg ac -> Effect (Array (Actor ac))
-- mkActors conf = do
--   for conf.actors
--     $ \a -> do
--         mbElem <- getHtmlElement a.nameId
--         let
--           mbHtmlElem = case mbElem of
--             Just el -> MaybeHTMLElem { unMaybeHtmlElem: Just el }
--             Nothing -> MaybeHTMLElem { unMaybeHtmlElem: Nothing }
--         pure
--           { nameId: a.nameId
--           , x: a.x
--           , y: a.y
--           , z: a.z
--           , visible : true
--           , angle : 0.0
--           , htmlElement: mbHtmlElem
--           , state: a.state
--           }

-- mkActors :: forall cfgst cfgac  ac. Config cfgst cfgac -> Effect (Array (Actor ac))
-- mkActors conf = pure []


initGame :: forall cfgst cfgac ui gm ac. 
  Config cfgst cfgac -> 
  gm -> 
  (Config cfgst cfgac -> Effect (Array (Actor ac))) ->
  Effect (Model gm ac ui)
initGame conf initialGameState mkActors = do
  let m = initialModelZeroTime initialGameState :: Model gm ac ui
  actors :: Array (Actor ac) <- mkActors conf -- :: Effect (Array ac)
  pure m{ actors = actors }
