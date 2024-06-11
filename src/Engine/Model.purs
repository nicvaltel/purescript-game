module Engine.Model
  ( Actor(..)
  , AppMod
  , AppModAff
  , AppModEffect
  , Model
  , ModelRec
  , NameId
  , appModEffectToAppModAff
  , appModToAppModAff
  , getModelRec
  , getNameId
  , initialModelZeroTime
  , mkActorsFromConfig
  , mkNewNameId
  , modmod
  , modmodAff
  , modmodEffect
  , putModel
  , putModelAff
  , putModelEffect
  )
  where

import Engine.Reexport

import Control.Monad.State (runStateT)
import Data.Map as M
import Engine.Config (Config)
import Engine.ResourceLoader (getHtmlElement)
import Engine.UserInput (UserInput, emptyUserInput)
import Engine.WebSocket.WSSignalChan as WS
import Record as R

type AppMod ac gm x = State (Model ac gm) x
type AppModEffect ac gm x = StateT (Model ac gm) Effect x
type AppModAff ac gm x = StateT (Model ac gm) Aff x

appModToAppModAff :: forall ac gm x. AppMod ac gm x -> AppModAff ac gm x
appModToAppModAff appMod = do
  m <- get
  let (Tuple result newState) = runState appMod m
  put newState
  pure result

appModEffectToAppModAff :: forall ac gm x. AppModEffect ac gm x -> AppModAff ac gm x
appModEffectToAppModAff appModEffect = do
  m <- get
  (Tuple result newState) <- liftEffect $ runStateT appModEffect m
  put newState
  pure result


newtype NameId = NameId String

instance showNameId :: Show NameId where
  show (NameId nameId) = nameId

derive instance eqNameId :: Eq NameId
derive instance ordNameId :: Ord NameId

getNameId :: NameId -> String
getNameId (NameId nameId) = nameId

newtype Actor ac = Actor {
    nameId :: NameId
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
  show (Actor actor) = show $ 
    R.modify (Proxy :: Proxy "htmlElement") (\el -> if isJust el then "Just HtmlElem" else "Nothing" ) actor


type ModelRec ac gm =
    { gameStepNumber :: Int
    , screenWidth :: Number
    , screenHeight :: Number
    , lastUpdateTime :: Instant
    , actors :: Map NameId (Actor ac)
    , lastActorId :: Int
    , recentlyAddedActors :: Array NameId
    , recentlyDeletedActors :: Array NameId
    , gameState :: gm
    , userInput :: UserInput
    , prevUserInput :: UserInput
    , wsIn :: Array WS.WSMessage 
    , wsOut :: Array WS.WSMessage
    , seed :: Seed
    }

newtype Model ac gm = Model (ModelRec ac gm)

-- derive instance newtypeModel :: Newtype (Model ac gm) _

instance showModel :: (Show ac, Show gm) => Show (Model ac gm) where
  show (Model m) =  
    foldr (\str acc -> acc <> "\t" <> str <> "\n") "MODEL:\n"
      $ [ "gameStepNumber " <> show m.gameStepNumber
        , "screenWidth " <> show m.screenWidth
        , "screenHeight " <> show m.screenHeight
        , "lastUpdateTime " <> show m.lastUpdateTime
        , "actors " <> (intercalate ", " $ map show m.actors)
        , "recentlyAddedActors" <> show (map getNameId m.recentlyAddedActors)
        , "recentlyDeletedActors" <> show (map getNameId m.recentlyDeletedActors)
        , "gameState" <> show (m.gameState)
        , "currentUserInput" <> show (m.userInput)
        , "prevUserInput" <> show (m.prevUserInput)
        , "wsIn" <> show (m.wsIn)
        , "wsOut" <> show (m.wsOut)
        ]

getModelRec :: forall ac gm. Model ac gm -> ModelRec ac gm
getModelRec (Model m) = m

modmod :: forall ac gm. (ModelRec ac gm -> ModelRec ac gm) -> AppMod ac gm Unit
modmod f = modify_ (\(Model m) -> Model (f m))

modmodEffect :: forall ac gm. (ModelRec ac gm -> ModelRec ac gm) -> AppModEffect ac gm Unit
modmodEffect f = modify_ (\(Model m) -> Model (f m))

modmodAff :: forall ac gm. (ModelRec ac gm -> ModelRec ac gm) -> AppModAff ac gm Unit
modmodAff f = modify_ (\(Model m) -> Model (f m))


putModel :: forall ac gm. Model ac gm -> AppMod ac gm Unit
putModel model = modify_ (\_ -> model)

putModelEffect :: forall ac gm. Model ac gm -> AppModEffect ac gm Unit
putModelEffect model = modify_ (\_ -> model)

putModelAff :: forall ac gm. Model ac gm -> AppModAff ac gm Unit
putModelAff model = modify_ (\_ -> model)

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
        , lastActorId: 0
        , recentlyAddedActors : []
        , recentlyDeletedActors : []
        , gameState
        , userInput : emptyUserInput
        , prevUserInput : emptyUserInput
        , wsIn : []
        , wsOut: []
        , seed: mkSeed 0
        }

mkNewNameId :: forall ac gm. AppMod ac gm NameId
mkNewNameId = do
  m <- getModelRec <$> get
  modmod $ \mr -> mr{lastActorId = mr.lastActorId + 1}
  pure (NameId ("ac_" <> show m.lastActorId))

mkActorsFromConfig :: forall ac gm. 
  Config ac gm -> 
  (gm -> ac -> ac) ->
  Effect (Map NameId (Actor ac))
mkActorsFromConfig conf mkActorData = do
  actorsArr <- for conf.actors
    $ \a -> do
        mbElem <- getHtmlElement a.nameId
        pure $ Tuple (NameId a.nameId) (Actor
          { nameId: (NameId a.nameId)
          , x: a.x
          , y: a.y
          , z: a.z
          , visible : true
          , angle : 0.0
          , cssClass : a.cssClass
          , imageSource : a.imageSource
          , htmlElement: mbElem
          , data: mkActorData conf.state a.data
          })
  pure $ M.fromFoldable actorsArr
