module Engine.Render.Render (render) where

import Prelude
import Engine.Config (Config)
import Data.Number (floor)
import Data.Traversable (for)
import Effect (Effect)
import Effect.Console (log)
import Engine.Model (Model, showModel)

type ActorObj
  = { id :: String, css :: String, baseX :: Number, baseY :: Number, x :: Number, y :: Number }

foreign import _renderObject :: ActorObj -> Effect Unit

render :: forall gm ac ui cfg. Show gm => Show ac => Config cfg -> Model gm ac ui -> Effect Unit
render conf m = do
  when conf.debugModel $ log (showModel m)
  _ <-
    for m.actors
      $ \actor -> do
          let
            actorObj =
              { id: actor.nameId
              , css: ""
              , baseX: 0.0
              , baseY: 0.0
              , x: floor $ actor.x
              , y: floor $ actor.y
              }
          _renderObject actorObj
  pure unit
