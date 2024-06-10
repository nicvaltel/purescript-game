module Engine.Model
  ( Actor(..)
  , Model(..)
  , initialModelZeroTime
  )
  where

import Engine.Reexport

import Data.Map as M
import Data.String as S
import Engine.UserInput (UserInput, emptyUserInput)
import Engine.WebSocket.WSSignalChan as WS

newtype Actor ac = Actor {
    nameId :: String
  , x :: Number
  , y :: Number
  , z :: Int
  , visible :: Boolean
  , angle :: Number
  , cssClass :: String
  , imageSource :: String
  , htmlElement :: Maybe HTMLElement 
  , data :: ac
}

derive instance newtypeActor :: Newtype (Actor ac) _

instance showActor :: Show ac => Show (Actor ac) where
  show (Actor actor) = 
    let str = show $ delete (Proxy :: Proxy "htmlElement") actor
    in  (S.take (S.length str - 1) str) 
        <> ", " <> "htmlElement: " 
        <> (if (isJust actor.htmlElement) then "Just HtmlElem" else "Nothing") 
        <> " }"


newtype Model ac gm = Model
    { gameStepNumber :: Int
    , screenWidth :: Number
    , screenHeight :: Number
    , lastUpdateTime :: Instant
    , actors :: Map NameId (Actor ac)
    , recentlyAddedActors :: Array NameId
    , recentlyDeletedActors :: Array NameId
    , gameState :: gm
    , userInput :: UserInput
    , prevUserInput :: UserInput
    , wsIn :: Array WS.WSMessage 
    , wsOut :: Array WS.WSMessage
    , seed :: Seed
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
        , seed: mkSeed 0
        }
