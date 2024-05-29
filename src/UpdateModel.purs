module UpdateModel where

import Prelude
import Model (Model, Actor)
import Types (Time)


moveActor :: Time -> Actor -> Actor
moveActor dt actor = 
    let newX = actor.x + dt * actor.vx
        newY = actor.y + dt * actor.vy
     in actor {
            x = if newX > 1300.0 then newX - 1300.0 else newX,
            y = if newY > 800.0 then newY - 800.0 else newY
        }

updateModel :: Time -> Model -> Model
updateModel dt m = 
    let newActors = map (moveActor dt) m.actors
     in m{actors = newActors}