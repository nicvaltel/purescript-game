module InitGame where

import Prelude

import Engine.Config (Config)
import Engine.Model (Actor, Model, initialModelZeroTime)


actorBall :: Actor
actorBall = 
  { name : "jupiter"
    , x : 17.0
    , y : 22.0
    , vx : 31.0/100.0
    , vy : 23.0/100.0
    , spriteName : "jupiter"
    }
populateActors :: Model -> Model
populateActors m = m {actors = [actorBall]}

initGame :: Config -> Model
initGame conf = populateActors initialModelZeroTime




-- initGame :: Effect Model
-- initGame = do
--   currentTime <- now
--   pure $ populateActors $ initialModel currentTime
