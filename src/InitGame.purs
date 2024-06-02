module InitGame
  ( ActorState
  , GameActor
  , GameModel
  , GameState
  , initGame
  )
  where

import Prelude
import Engine.Config (Config)
import Engine.Model (Actor, Model, initialModelZeroTime)

type GameState = Int
type ActorState = {
  vx :: Number,
  vy :: Number
}
type GameModel = Model GameState ActorState
type GameActor = Actor ActorState

actorBalls :: Array GameActor
actorBalls =
  [ { nameId: "black_stone"
    , x: 17.0
    , y: 22.0
    , state : {
        vx: 31.0 / 100.0
      , vy: 23.0 / 100.0
      }
    },
    { nameId: "white_stone"
    , x: 63.0
    , y: 57.0
    , state : {
        vx: 18.0 / 100.0
      , vy: 27.0 / 100.0
      }
    },
    { nameId: "black_hole"
    , x: 0.0
    , y: 0.0
    , state : {
        vx: 0.0 / 100.0
      , vy: 0.0 / 100.0
      }
    }
  ]

populateActors :: GameModel -> GameModel
populateActors m = m { actors = actorBalls }

initGame :: Config -> GameModel
initGame conf = populateActors $ initialModelZeroTime 0

-- initGame :: Effect Model
-- initGame = do
--   currentTime <- now
--   pure $ populateActors $ initialModel currentTime
