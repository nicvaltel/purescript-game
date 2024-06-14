module Engine.Model
  ( Actor(..)
  , AppMod
  , AppModAff
  , AppModEffect
  , Model
  , ModelRec
  , NameId
  , actorMock
  , appModEffectToAppModAff
  , appModToAppModAff
  , appModToAppModEffect
  , checkActorNameId
  , class ActorContainer
  , getAllActors
  , getModelRec
  , getNameId
  , getRandom
  , initialModelZeroTime
  , lookupActor
  , mkActorsFromConfig
  , mkNewNameId
  , mkUniqueNameId
  , modmod
  , modmodAff
  , modmodEffect
  , putModel
  , putModelAff
  , putModelEffect
  , updateActor
  )
  where

import Engine.Reexport
import Control.Monad.State (runStateT)
import Data.List (List)
import Data.Map as M
import Engine.Config (Config)
import Engine.Random.PseudoRandom (class Random)
import Engine.ResourceLoader (getHtmlElement)
import Engine.UserInput (UserInput, emptyUserInput)
import Engine.WebSocket.WSSignalChan as WS
import Record as R

type AppMod gm x = State (Model gm) x
type AppModEffect gm x = StateT (Model gm) Effect x
type AppModAff gm x = StateT (Model gm) Aff x

appModToAppModAff :: forall gm x. AppMod gm x -> AppModAff gm x
appModToAppModAff appMod = do
  m <- get
  let (Tuple result newState) = runState appMod m
  put newState
  pure result

appModToAppModEffect :: forall gm x. AppMod gm x -> AppModEffect gm x
appModToAppModEffect appMod = do
  m <- get
  let (Tuple result newState) = runState appMod m
  put newState
  pure result

appModEffectToAppModAff :: forall gm x. AppModEffect gm x -> AppModAff gm x
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

instance decodeJsonNameId :: DecodeJson NameId where
  decodeJson json = do
    string <- decodeJson json
    note (TypeMismatch "NameId") (nameIdFromString string)
    where
      nameIdFromString :: String -> Maybe NameId
      nameIdFromString str = Just (NameId str)


newtype Actor ac = Actor {
    nameId :: NameId
  , x :: Number
  , y :: Number
  , width :: Number
  , height :: Number
  , z :: Int
  , visible :: Boolean
  , angle :: Number
  , cssClass :: String
  , imageSource :: String
  , htmlElement :: Maybe HTMLElement 
  , data :: ac
}

actorMock :: Actor Unit 
actorMock = Actor
        {   nameId: mkUniqueNameId "actor_mock",
            x: 0.0,
            y: 0.0,
            z: 0,
            width : 0.0,
            height : 0.0,
            visible : false,
            angle : 0.0,
            htmlElement : Nothing,
            cssClass : "",
            imageSource: "",
            data: unit
            }

checkActorNameId :: forall ac. NameId -> Actor ac -> Boolean
checkActorNameId nameId (Actor aRec) = nameId == aRec.nameId

-- instance decodeJsonActor :: DecodeJsonField ac => DecodeJson (Actor ac) where
--   decodeJson json = do
--     obj <- decodeJson json -- attempts to decode the JSON value as an object.
--     t <- obj .: "type" -- extracts the "type" field from the JSON object.
--     case t of
--       "actor" -> Actor <$> (obj .: "data") -- This pattern matches the "type" field to determine which constructor to use (ActorBall or ActorGun).
--       -- "gameState" -> undefined
--       _      -> Left $ TypeMismatch $ "Unknown Actor type: " <> t

derive instance newtypeActor :: Newtype (Actor ac) _

instance showActor :: Show ac => Show (Actor ac) where
  show (Actor actor) = show $ 
    R.modify (Proxy :: Proxy "htmlElement") (\el -> if isJust el then "Just HtmlElem" else "Nothing" ) actor


type ModelRec gm =
    { 
    game :: gm
    , act :: {
      recentlyAddedActors :: Array (Tuple NameId String) -- String is a clue to ActorContainer functions, where to find this actor
    , recentlyDeletedActors :: Array NameId
    }
    , io :: {
      userInput :: UserInput
    , prevUserInput :: UserInput
    , wsIn :: Array WS.WSMessage 
    , wsOut :: Array WS.WSMessage
    }
    , sys :: {
        gameStepNumber :: Int
      , screenWidth :: Number
      , screenHeight :: Number
      , lastUpdateTime :: Instant
      , lastActorId :: Int
      , seed :: Seed
      }
    }

newtype Model gm = Model (ModelRec gm)

-- derive instance newtypeModel :: Newtype (Model gm) _

instance showModel :: Show gm => Show (Model gm) where
  show (Model m) =  
    foldr (\str acc -> acc <> "\t" <> str <> "\n") "MODEL:\n"
      $ [ "gameStepNumber " <> show m.sys.gameStepNumber
        , "screenWidth " <> show m.sys.screenWidth
        , "screenHeight " <> show m.sys.screenHeight
        , "lastUpdateTime " <> show m.sys.lastUpdateTime
        , "recentlyAddedActors" <> show m.act.recentlyAddedActors
        , "recentlyDeletedActors" <> show m.act.recentlyDeletedActors
        , "gameState" <> show (m.game)
        , "currentUserInput" <> show (m.io.userInput)
        , "prevUserInput" <> show (m.io.prevUserInput)
        , "wsIn" <> show (m.io.wsIn)
        , "wsOut" <> show (m.io.wsOut)
        ]

getModelRec :: forall gm. Model gm -> ModelRec gm
getModelRec (Model m) = m

modmod :: forall gm. (ModelRec gm -> ModelRec gm) -> AppMod gm Unit
modmod f = modify_ (\(Model m) -> Model (f m))

modmodEffect :: forall gm. (ModelRec gm -> ModelRec gm) -> AppModEffect gm Unit
modmodEffect f = modify_ (\(Model m) -> Model (f m))

modmodAff :: forall gm. (ModelRec gm -> ModelRec gm) -> AppModAff gm Unit
modmodAff f = modify_ (\(Model m) -> Model (f m))


putModel :: forall gm. Model gm -> AppMod gm Unit
putModel model = modify_ (\_ -> model)

putModelEffect :: forall gm. Model gm -> AppModEffect gm Unit
putModelEffect model = modify_ (\_ -> model)

putModelAff :: forall gm. Model gm -> AppModAff gm Unit
putModelAff model = modify_ (\_ -> model)

getRandom :: forall gm x. Random x => AppMod gm x
getRandom = do
  (Model mr) <- get
  let randomPair = random mr.sys.seed :: RandomPair x
  modify_ (\(Model m) -> Model m{sys{seed = randomPair.newSeed}})
  pure randomPair.newVal

-- TODO setup Model with config
initialModelZeroTime :: forall gm. gm -> Model gm
initialModelZeroTime gameState =
  unsafePartial
    $ let
        Just time = instant (Milliseconds 0.0)
      in Model
        { 
          game : gameState
        , act : {
          recentlyAddedActors : []
        , recentlyDeletedActors : []
        }
        , io : {
          userInput : emptyUserInput
        , prevUserInput : emptyUserInput
        , wsIn : []
        , wsOut : []
        }
        , sys : {
            gameStepNumber : 0
          , screenWidth : 0.0 -- TODO setup this
          , screenHeight : 0.0
          , lastUpdateTime : time 
          , lastActorId : 0
          , seed : mkSeed 0
          }
        }

mkNewNameId :: forall gm. AppMod gm NameId
mkNewNameId = do
  m <- getModelRec <$> get
  modmod $ \mr -> mr{sys{lastActorId = mr.sys.lastActorId + 1}}
  pure (NameId ("ac_" <> show m.sys.lastActorId))

mkUniqueNameId :: String -> NameId
mkUniqueNameId nameId = NameId nameId

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
          , width : 0.0
          , height : 0.0
          , visible : true
          , angle : 0.0
          , cssClass : a.cssClass
          , imageSource : a.imageSource
          , htmlElement: mbElem
          , data: mkActorData conf.state a.data
          })
  pure $ M.fromFoldable actorsArr


class ActorContainer ac gm where
  getAllActors :: Model gm -> List (Actor ac)
  updateActor :: NameId -> Maybe String -> (Actor ac -> Actor ac) -> AppMod gm Unit
  lookupActor :: NameId -> Maybe String -> Model gm -> Maybe (Actor ac)