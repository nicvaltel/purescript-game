module Engine.Render.Render (render) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Number (floor)
import Data.Traversable (for)
import Effect (Effect)
import Effect.Console (log)
import Engine.Config (Config)
import Engine.Model (class Actor, MaybeHTMLElem(..), Model, getActorAngle, getActorHhtmlElement, getActorX, getActorY, getActorZ, showModel)
import Engine.ResourceLoader (getHtmlElement)
import Web.HTML (HTMLElement)

type ActorObj
  = { elem :: HTMLElement, css :: String, baseX :: Number, baseY :: Number, x :: Number, y :: Number, z :: Int, angle :: Number }

foreign import _renderObject :: ActorObj -> Effect Unit

render :: forall gm ac ui cfgst cfgac. 
  Show gm => 
  Show ac => 
  Actor ac => 
  Config cfgst cfgac -> 
  Model gm ac ui -> 
  Effect Unit
render conf m = do
  when conf.debugModel $ log (showModel m)
  _ <-
    for m.actors
      $ \actor -> do
          let mbElem = getActorHhtmlElement actor
          case mbElem of
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
                  , x: floor $ getActorX actor
                  , y: floor $ getActorY actor
                  , z : getActorZ actor
                  , angle : floor $ getActorAngle actor
                  }
