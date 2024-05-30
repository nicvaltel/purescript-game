module InitGame
  ( initGame
  )
  where

import Prelude
import Engine.Config (Config)
import Engine.Model (Actor, Model, initialModelZeroTime)

actorBalls :: Array Actor
actorBalls =
  [
  { name: "jupiter"
  , x: 17.0
  , y: 22.0
  , vx: 31.0 / 100.0
  , vy: 23.0 / 100.0
  , spriteName: "jupiter"
  },
  {
    name: "billiard"
  , x: 63.0
  , y: 57.0
  , vx: 17.0 / 100.0
  , vy: 27.0 / 100.0
  , spriteName: "billiard"
  },
  {
    name: "white"
  , x: 123.0
  , y: 17.0
  , vx: 20.0 / 100.0
  , vy: 13.0 / 100.0
  , spriteName: "white"
  }
  ]
populateActors :: Model -> Model
populateActors m = m { actors = actorBalls}

initGame :: Config -> Model
initGame conf = populateActors initialModelZeroTime

-- initGame :: Effect Model
-- initGame = do
--   currentTime <- now
--   pure $ populateActors $ initialModel currentTime
