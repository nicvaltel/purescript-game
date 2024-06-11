module Engine.InitGame
  ( initGame
  ) where

import Prelude

import Data.Map (Map)
import Data.Map as M
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Engine.Config (Config)
import Engine.Model (Actor(..), Model(..), NameId, initialModelZeroTime, mkActorsFromConfig)


-- mkActorsFromConfig :: forall ac gm. 
--   Config ac gm -> 
--   (gm -> ac -> ac) ->
--   Effect (Map NameId (Actor ac))
-- mkActorsFromConfig conf mkActorData = do
--   actorsArr <- for conf.actors
--     $ \a -> do
--         mbElem <- getHtmlElement a.nameId
--         pure $ Tuple a.nameId (Actor
--           { nameId: a.nameId
--           , x: a.x
--           , y: a.y
--           , z: a.z
--           , visible : true
--           , angle : 0.0
--           , cssClass : a.cssClass
--           , imageSource : a.imageSource
--           , htmlElement: mbElem
--           , data: mkActorData conf.state a.data
--           })
--   pure $ M.fromFoldable actorsArr

initGame :: forall ac gm. 
  Config ac gm -> 
  gm -> 
  (gm -> ac -> ac) ->
  Effect (Model ac gm)
initGame conf initialGameState mkActorData = do
  let (Model m) = initialModelZeroTime initialGameState :: Model ac gm
  actors :: Map NameId (Actor ac) <- mkActorsFromConfig conf mkActorData
  pure $ Model m{ actors = actors }
