module Engine.Model
  ( Actor(..)
  , ActorRec
  , AddedActor
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
  , getActorData
  , getActorRec
  , getAllActors
  , getModelRec
  , getNameId
  , getRandom
  , initialModelZeroTime
  , lookupActor
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
import Engine.Random.PseudoRandom (class Random)
import Engine.UserInput (UserInput, emptyUserInput)
import Engine.WebSocket.WSSignalChan as WS
import Record as R
import Web.HTML.HTMLMediaElement(HTMLMediaElement)

type AppMod ac gm x = State (Model ac gm) x
type AppModEffect ac gm x = StateT (Model ac gm) Effect x
type AppModAff ac gm x = StateT (Model ac gm) Aff x

appModToAppModAff :: forall ac gm x. AppMod ac gm x -> AppModAff ac gm x
appModToAppModAff appMod = do
  m <- get
  let (Tuple result newState) = runState appMod m
  put newState
  pure result

appModToAppModEffect :: forall ac gm x. AppMod ac gm x -> AppModEffect ac gm x
appModToAppModEffect appMod = do
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

instance decodeJsonNameId :: DecodeJson NameId where
  decodeJson json = do
    string <- decodeJson json
    note (TypeMismatch "NameId") (nameIdFromString string)
    where
      nameIdFromString :: String -> Maybe NameId
      nameIdFromString str = Just (NameId str)

type ActorRec ac = {
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

newtype Actor ac = Actor (ActorRec ac)

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

getActorData :: forall ac. Actor ac -> ac
getActorData (Actor a) = a.data

getActorRec :: forall ac. Actor ac -> ActorRec ac
getActorRec (Actor r) = r

checkActorNameId :: forall ac. NameId -> Actor ac -> Boolean
checkActorNameId nameId (Actor aRec) = nameId == aRec.nameId

derive instance newtypeActor :: Newtype (Actor ac) _

instance showActor :: Show ac => Show (Actor ac) where
  show (Actor actor) = show $ 
    R.modify (Proxy :: Proxy "htmlElement") (\el -> if isJust el then "Just HtmlElem" else "Nothing" ) actor


type AddedActor = 
  { nameId :: NameId
  , parentElemId :: NameId
  , clue :: String -- String is a clue to ActorContainer functions, where to find this actor
  }

type ModelRec ac gm =
    { 
    game :: gm
    , act :: {
      recentlyAddedActors :: Array AddedActor
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
    , actorNothing :: Maybe (Actor ac) --need for Model contain ac type
    , audioElemsToPlay :: Array HTMLMediaElement
    }

newtype Model ac gm = Model (ModelRec ac gm)

-- derive instance newtypeModel :: Newtype (Model ac gm) _ -- TODO use unwrap instead of GetModelRec

instance showModel :: (Show ac, Show gm) => Show (Model ac gm) where
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

-- Q: Why not unwrap instead of getModelRec?
-- A: An export for Model hides data constructors but the type declares an instance of Data.Newtype.Newtype. Such instance allows to match and construct values of this type, effectively making the constructors public.
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

getRandom :: forall ac gm x. Random x => AppMod ac gm x
getRandom = do
  (Model mr) <- get
  let randomPair = random mr.sys.seed :: RandomPair x
  modify_ (\(Model m) -> Model m{sys{seed = randomPair.newSeed}})
  pure randomPair.newVal

initialModelZeroTime :: forall ac gm. Int -> Number -> Number -> gm -> Model ac gm
initialModelZeroTime seed screenWidth screenHeight gameState =
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
          , screenWidth : screenWidth
          , screenHeight : screenHeight
          , lastUpdateTime : time 
          , lastActorId : 0
          , seed : mkSeed seed
          }
        , actorNothing : Nothing
        , audioElemsToPlay : []
        }

mkNewNameId :: forall ac gm. AppMod ac gm NameId
mkNewNameId = do
  m <- getModelRec <$> get
  modmod $ \mr -> mr{sys{lastActorId = mr.sys.lastActorId + 1}}
  pure (NameId ("ac_" <> show m.sys.lastActorId))

mkUniqueNameId :: String -> NameId
mkUniqueNameId nameId = NameId nameId


class ActorContainer ac gm where
  getAllActors :: Model ac gm -> List (Actor ac)
  updateActor :: NameId -> Maybe String -> (Actor ac -> Actor ac) -> AppMod ac gm Unit
  lookupActor :: NameId -> Maybe String -> Model ac gm -> Maybe (Actor ac)