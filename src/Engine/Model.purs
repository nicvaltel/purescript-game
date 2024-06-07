module Engine.Model
  ( Actor(..)
  , Model(..)
  , initialModelZeroTime
  , showModel
  )
  where

import Prelude

import Data.Foldable (intercalate)
import Data.DateTime.Instant (Instant, instant)
import Data.Foldable (foldr)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Engine.UserInput (UserInput, emptyUserInput)
import Partial.Unsafe (unsafePartial)
import Web.HTML (HTMLElement)

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

showActor :: forall ac. Show ac => Actor ac -> String
showActor actor = 
  foldr (\str acc -> acc <> "\t" <> str <> "\n") "ACTOR:\n"
    $ [ "nameId" <> show actor.nameId
      , "x" <> show actor.x
      , "y" <> show actor.y
      , "z" <> show actor.z
      , "visible" <> show actor.visible
      , "angle" <> show actor.angle
      , "data" <> show actor.data
      ]

type Model ac gm ui = -- Actor ac =>
    { gameStepNumber :: Int
    , screenWidth :: Number
    , screenHeight :: Number
    , lastUpdateTime :: Instant
    , actors :: Array (Actor ac)
    , gameState :: gm
    , prevUserInput :: UserInput ui
    }


showModel :: forall ac gm ui. Show gm => Show ac => Model ac gm ui -> String
showModel m =
  foldr (\str acc -> acc <> "\t" <> str <> "\n") "MODEL:\n"
    $ [ "gameStepNumber " <> show m.gameStepNumber
      , "screenWidth " <> show m.screenWidth
      , "screenHeight " <> show m.screenHeight
      , "lastUpdateTime " <> show m.lastUpdateTime
      , "actors " <> (intercalate ", " $ map showActor m.actors)
      , "gameState" <> show (m.gameState)
      ]

initialModel :: forall ac gm ui. Instant -> gm -> Model ac gm ui
initialModel currentTime gameState = (initialModelZeroTime gameState) { lastUpdateTime = currentTime }


-- TODO what is it???
initialModelZeroTime :: forall ac gm ui. gm -> Model ac gm ui
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
