module Render.Render (render) where

import Prelude

import Config (Config)
import Data.Number (floor)
import Data.Traversable (for)
import Effect (Effect)
import Effect.Console (log)
import Model (Model, showModel)

type ActorObj = {id :: String, css :: String, baseX :: Number, baseY :: Number, x :: Number, y :: Number}

foreign import _renderObject :: ActorObj ->  Effect Unit

render :: Config -> Model -> Effect Unit
render conf m = do
        when conf.debug $ log (showModel m)
        _ <- for m.actors $ \actor -> do
              let actorObj = {
                id : actor.name,
                css : "",
                baseX : 0.0,
                baseY : 0.0,
                x : floor $ actor.x,
                y : floor $ actor.y
              }
              _renderObject actorObj
        pure unit
