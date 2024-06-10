module Engine.Model
  ( Actor(..)
  , Model(..)
  , initialModelZeroTime
  )
  where

import Engine.Reexport

import Engine.UserInput (UserInput, emptyUserInput)
import Engine.WebSocket.WSSignalChan as WS
import Data.Map as M

newtype Actor ac = Actor {
    nameId :: String
  , x :: Number
  , y :: Number
  , z :: Int
  , visible :: Boolean
  , angle :: Number
  , css :: String
  , imageSource :: String
  , htmlElement :: Maybe HTMLElement 
  , data :: ac
}

derive instance newtypeActor :: Newtype (Actor ac) _

instance showActor :: Show ac => Show (Actor ac) where
  show (Actor actor) = "{" <> 
    intercalate ", " 
        [ "nameId: " <> show actor.nameId
        , "x: " <> show actor.x
        , "y: " <> show actor.y
        , "z: " <> show actor.z
        , "visible: " <> show actor.visible
        , "angle: " <> show actor.angle
        , "imageSource" <> actor.imageSource
        , "htmlElement: " <> if (isJust actor.htmlElement) then "Just HtmlElem" else "Nothing"
        , "data: " <> show actor.data
        ]
    <> "}"

newtype Model ac gm = Model
    { gameStepNumber :: Int
    , screenWidth :: Number
    , screenHeight :: Number
    , lastUpdateTime :: Instant
    , actors :: Map NameId (Actor ac)
    , recentlyAddedActors :: Array (Actor ac)
    , recentlyDeletedActors :: Array (Actor ac)
    , gameState :: gm
    , userInput :: UserInput
    , prevUserInput :: UserInput
    , wsIn :: Array WS.WSMessage 
    , wsOut :: Array WS.WSMessage 
    }

derive instance newtypeModel :: Newtype (Model ac gm) _

instance showModel :: (Show ac, Show gm) => Show (Model ac gm) where
  show (Model m) =  
    foldr (\str acc -> acc <> "\t" <> str <> "\n") "MODEL:\n"
      $ [ "gameStepNumber " <> show m.gameStepNumber
        , "screenWidth " <> show m.screenWidth
        , "screenHeight " <> show m.screenHeight
        , "lastUpdateTime " <> show m.lastUpdateTime
        , "actors " <> (intercalate ", " $ map show m.actors)
        , "recentlyAddedActors" <> show (m.recentlyAddedActors)
        , "recentlyDeletedActors" <> show (m.recentlyDeletedActors)
        , "gameState" <> show (m.gameState)
        , "currentUserInput" <> show (m.userInput)
        , "prevUserInput" <> show (m.prevUserInput)
        , "wsIn" <> show (m.wsIn)
        , "wsOut" <> show (m.wsOut)
        ]

-- initialModel :: forall ac gm. Instant -> gm -> Model ac gm
-- initialModel currentTime gameState = 
--   let (Model m) = initialModelZeroTime gameState
--   in Model m{ lastUpdateTime = currentTime }

-- TODO setup Model with config
initialModelZeroTime :: forall ac gm. gm -> Model ac gm
initialModelZeroTime gameState =
  unsafePartial
    $ let
        Just time = instant (Milliseconds 0.0)
      in Model
        { gameStepNumber: 0
        , screenWidth: 0.0
        , screenHeight: 0.0
        , lastUpdateTime: time
        , actors: M.empty :: Map NameId (Actor ac)
        , recentlyAddedActors : []
        , recentlyDeletedActors : []
        , gameState
        , userInput : emptyUserInput
        , prevUserInput : emptyUserInput
        , wsIn : []
        , wsOut: []
        }
