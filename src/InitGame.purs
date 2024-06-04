module InitGame
  ( initGame
  )
  where

import Prelude

import Data.Maybe (Maybe(..))
import Engine.Model (MaybeHTMLElem(..), initialModelZeroTime)
import GameModel (GameActor, GameConfig, GameModel, GameState)

actorBalls :: Array GameActor
actorBalls =
  [ { nameId: "black_stone"
    , x: 17.0
    , y: 22.0
    , htmlElement : MaybeHTMLElem {unMaybeHtmlElem : Nothing}
    , state : {
        vx: 31.0 / 100.0
      , vy: 23.0 / 100.0
      }
    },
    { nameId: "white_stone"
    , x: 63.0
    , y: 57.0
    , htmlElement : MaybeHTMLElem {unMaybeHtmlElem : Nothing}
    , state : {
        vx: 18.0 / 100.0
      , vy: 27.0 / 100.0
      }
    },
    { nameId: "black_hole"
    , x: 0.0
    , y: 0.0
    , htmlElement : MaybeHTMLElem {unMaybeHtmlElem : Nothing}
    , state : {
        vx: 0.0 / 100.0
      , vy: 0.0 / 100.0
      }
    }
  ]

populateActors :: GameModel -> GameModel
populateActors m = m { actors = actorBalls }

initialGameState :: GameState
initialGameState = {
  gridSize : 0
}

initGame :: GameConfig -> GameModel
initGame conf = populateActors $ initialModelZeroTime initialGameState



-- initGame :: Effect Model
-- initGame = do
--   currentTime <- now
--   pure $ populateActors $ initialModel currentTime
