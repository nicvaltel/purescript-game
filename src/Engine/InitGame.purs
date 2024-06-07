module Engine.InitGame
  ( initGame
  ) where

import Prelude

import Data.Traversable (for)
import Effect (Effect)
import Engine.Config (Config)
import Engine.Model (Actor(..), Model(..), initialModelZeroTime)
import Engine.ResourceLoader (getHtmlElement)


mkActorsFromConfig :: forall ac gm. 
  Config ac gm -> 
  (gm -> ac -> ac) ->
  Effect (Array (Actor ac))
mkActorsFromConfig conf mkActorData = do
  for conf.actors
    $ \a -> do
        mbElem <- getHtmlElement a.nameId
        pure $ Actor
          { nameId: a.nameId
          , x: a.x
          , y: a.y
          , z: a.z
          , visible : true
          , angle : 0.0
          , htmlElement: mbElem
          , data: mkActorData conf.state a.data
          }

initGame :: forall ac gm ui. 
  Config ac gm -> 
  gm -> 
  (gm -> ac -> ac) ->
  Effect (Model ac gm ui)
initGame conf initialGameState mkActorData = do
  let (Model m) = initialModelZeroTime initialGameState :: Model ac gm ui
  actors :: Array (Actor ac) <- mkActorsFromConfig conf mkActorData
  pure $ Model m{ actors = actors }
