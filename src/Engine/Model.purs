module Engine.Model
  ( Actor(..)
  , Model(..)
  , initialModelZeroTime
  , showModel
  )
  where

import Prelude

import Data.DateTime.Instant (Instant, instant)
import Data.Foldable (foldr)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Time (Millisecond)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Now (now)
import Engine.Config (Config)
import Engine.ResourceLoader (getHtmlElement)
import Engine.UserInput (UserInput, emptyUserInput)
import Engine.Utils.Utils (undefined)
import Partial.Unsafe (unsafePartial)
import Web.HTML (HTMLElement)


-- newtype MaybeHTMLElem = MaybeHTMLElem {unMaybeHtmlElem :: Maybe HTMLElement}

-- instance showMaybeHTMLElem :: Show MaybeHTMLElem where
--   show (MaybeHTMLElem mbElem) = case mbElem.unMaybeHtmlElem of
--       Nothing -> "Nothing"
--       Just _ -> "Just HtmlElement"


-- type Actor ac
--   = { nameId :: String
--     , x :: Number
--     , y :: Number
--     , z :: Int
--     , visible :: Boolean
--     , angle :: Number -- 0 is normal unrotated image
--     , htmlElement :: MaybeHTMLElem
--     , data :: ad
--     }

type Actor ac = {
    nameId :: String
  , x :: Number
  , y :: Number
  , z :: Int
  , visible :: Boolean
  , angle :: Number
  , htmlElement :: Maybe HTMLElement 
  , data :: ac
}

type Model gm ac ui = -- Actor ac =>
    { gameStepNumber :: Int
    , screenWidth :: Number
    , screenHeight :: Number
    , lastUpdateTime :: Instant
    , actors :: Array (Actor ac)
    , gameState :: gm
    , prevUserInput :: UserInput ui
    }


showModel :: forall gm ac ui. Show gm => Show ac => Model gm ac ui -> String
showModel m = ""
  -- foldr (\str acc -> acc <> "\t" <> str <> "\n") "MODEL:\n"
  --   $ [ "gameStepNumber " <> show m.gameStepNumber
  --     , "screenWidth " <> show m.screenWidth
  --     , "screenHeight " <> show m.screenHeight
  --     , "lastUpdateTime " <> show m.lastUpdateTime
  --     , "actors " <> show m.actors
  --     , "gameState" <> show (m.gameState)
  --     ]

initialModel :: forall gm ac ui.Instant -> gm -> Model gm ac ui
initialModel currentTime gameState = (initialModelZeroTime gameState) { lastUpdateTime = currentTime }

initialModelZeroTime :: forall gm ac ui. gm -> Model gm ac ui
initialModelZeroTime gameState =
  unsafePartial
    $ let
        Just time = instant (Milliseconds 0.0)
      in
        { gameStepNumber: 0
        , screenWidth: 150.0
        , screenHeight: 100.0
        , lastUpdateTime: time
        , actors: [] :: Array (Actor ac)
        , gameState
        , prevUserInput : emptyUserInput
        }
