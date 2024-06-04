module InitGame
  ( initGame
  ) where

import Prelude
import Data.Functor (mapFlipped)
import Data.Maybe (Maybe(..))
import Data.Traversable (for)
import Effect (Effect)
import Engine.Model (MaybeHTMLElem(..), initialModelZeroTime)
import Engine.ResourceLoader (getHtmlElement)
import GameModel (GameActor, GameModel, GameState, GameConfig)
import Data.Maybe (Maybe(..))

mkActors :: GameConfig -> Effect (Array GameActor)
mkActors conf = do
  for conf.actors
    $ \a -> do
        mbElem <- getHtmlElement a.nameId
        let
          mbHtmlElem = case mbElem of
            Just el -> MaybeHTMLElem { unMaybeHtmlElem: Just el }
            Nothing -> MaybeHTMLElem { unMaybeHtmlElem: Nothing }
        pure
          { nameId: a.nameId
          , x: a.x
          , y: a.y
          , htmlElement: mbHtmlElem
          , state:
              { vx: a.state.vx
              , vy: a.state.vy
              }
          }

populateActors :: GameConfig -> GameModel -> Effect GameModel
populateActors conf m = do
  actors <- mkActors conf
  pure m{ actors = actors }

initialGameState :: GameState
initialGameState =
  { gridSize: 0
  }

initGame :: GameConfig -> Effect GameModel
initGame conf = populateActors conf $ initialModelZeroTime initialGameState
