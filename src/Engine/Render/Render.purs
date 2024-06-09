module Engine.Render.Render (render) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Number (floor)
import Data.Traversable (for)
import Effect (Effect)
import Effect.Console (log)
import Engine.Config (Config)
import Engine.Model (Actor(..), Model(..))
import Web.HTML (HTMLElement)

type ActorObj
  = { elem :: HTMLElement, css :: String, baseX :: Number, baseY :: Number, x :: Number, y :: Number, z :: Int, angle :: Number }

foreign import _renderObject :: ActorObj -> Effect Unit

render :: forall ac gm. 
  Show gm => 
  Show ac => 
  Config ac gm -> 
  Model ac gm -> 
  Effect Unit
render conf model@(Model m) = do
  when conf.debugModel $ log (show model)
  _ <-
    for m.actors
      $ \(Actor actor) -> do
          case actor.htmlElement of
            Nothing -> pure unit
            Just el ->
              let actorObj = mkActorObj el actor
              in _renderObject actorObj
  pure unit
  where
    mkActorObj el actor = do
                  { elem: el
                  , css: ""
                  , baseX: 0.0
                  , baseY: 0.0
                  , x: floor $ actor.x
                  , y: floor $ actor.y
                  , z : actor.z
                  , angle : floor $ actor.angle
                  }
