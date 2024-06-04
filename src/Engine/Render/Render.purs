module Engine.Render.Render (render) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Number (floor)
import Data.Traversable (for)
import Effect (Effect)
import Effect.Console (log)
import Engine.Config (Config)
import Engine.Model (MaybeHTMLElem(..), Model, Actor, showModel)
import Web.HTML (HTMLElement)

type ActorObj
  = { elem :: HTMLElement, css :: String, baseX :: Number, baseY :: Number, x :: Number, y :: Number, z :: Int }

foreign import _renderObject :: ActorObj -> Effect Unit

render :: forall gm ac ui cfg. Show gm => Show ac => Config cfg ac -> Model gm ac ui -> Effect Unit
render conf m = do
  when conf.debugModel $ log (showModel m)
  _ <-
    for m.actors
      $ \actor -> do
          let (MaybeHTMLElem mbElem) = actor.htmlElement
          case mbElem.unMaybeHtmlElem of
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
                  }
