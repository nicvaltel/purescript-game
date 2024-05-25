module GameData where

import Prelude
import GameModel
import Types


-- actorBall :: Actor
-- actorBall = 
--   { name : "actor_red_ball"
--     , x : 17.0
--     , y : 22.0
--     , vx : 35.0
--     , vy : 25.0
--     , spriteName : "red_ball"
--     }


-- populateActors :: Model -> Model
-- populateActors m = m {actors = [actorBall]}


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