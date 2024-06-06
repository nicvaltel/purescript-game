module Engine.InitGame
  ( initGame
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Traversable (for)
import Effect (Effect)
import Engine.Model (Model, Actor, MaybeHTMLElem(..), initialModelZeroTime)
import Engine.ResourceLoader (getHtmlElement)
import Engine.Config(Config)

mkActors :: forall cfg ac. Config cfg ac -> Effect (Array (Actor ac))
mkActors conf = do
  for conf.actors
    $ \a -> do
        mbElem <- getHtmlElement a.nameId
        let
          mbHtmlElem = case mbElem of
            Just el -> MaybeHTMLElem { unMaybeHtmlElem: Just el }
            Nothing -> MaybeHTMLElem { unMaybeHtmlElem: Nothing }
        pure
          { nameId: a.nameId
          , x: a.x
          , y: a.y
          , z: a.z
          , visible : true
          , angle : 0.0
          , htmlElement: mbHtmlElem
          , state: a.state
          }

initGame :: forall ui gm ac cfg. Config cfg ac-> gm -> Effect (Model gm ac ui)
initGame conf initialGameState = do
  let m = initialModelZeroTime initialGameState
  actors <- mkActors conf
  pure m{ actors = actors }
