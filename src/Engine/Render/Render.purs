module Engine.Render.Render (render) where

import Engine.Reexport
import Engine.Config (Config)
import Engine.Model (Actor(..), Model, getModelRec)


type ActorObj
  = { elem :: HTMLElement, cssClass :: String, baseX :: Number, baseY :: Number, x :: Number, y :: Number, z :: Int, angle :: Number }

foreign import _renderObject :: ActorObj -> Effect Unit

render :: forall ac gm. 
  Show gm => 
  Show ac => 
  Config ac gm -> 
  Model ac gm -> 
  Effect Unit
render conf model = do
  let m = getModelRec model
  when conf.debugModel $ log (show model)
  _ <-
    for m.actors
      $ \(Actor actor) -> do
          when actor.visible $
            case actor.htmlElement of
              Nothing -> pure unit
              Just el ->
                let actorObj = mkActorObj el actor
                in _renderObject actorObj
  pure unit
  where
    mkActorObj el actor = do
                  { elem: el
                  , cssClass: actor.cssClass
                  , baseX: 0.0
                  , baseY: 0.0
                  , x: floor $ actor.x
                  , y: floor $ actor.y
                  , z : actor.z
                  , angle : floor $ actor.angle
                  }
