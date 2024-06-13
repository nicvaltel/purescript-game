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
  , getRandom
  , initialModelZeroTime
  , mkActorsFromConfig
  , mkNewNameId
  , mkUniqueNameId
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
import Engine.Random.PseudoRandom (class Random)
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

derive instance newtypeActor :: Newtype (Actor ac) _

instance showActor :: Show ac => Show (Actor ac) where
  show (Actor actor) = show $ 
    R.modify (Proxy :: Proxy "htmlElement") (\el -> if isJust el then "Just HtmlElem" else "Nothing" ) actor


type ModelRec ac gm =
    { 
    game :: gm
    , act :: {
      actors :: Map NameId (Actor ac)
    , recentlyAddedActors :: Array NameId
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

newtype Model ac gm = Model (ModelRec ac gm)

-- derive instance newtypeModel :: Newtype (Model ac gm) _

instance showModel :: (Show ac, Show gm) => Show (Model ac gm) where
  show (Model m) =  
    foldr (\str acc -> acc <> "\t" <> str <> "\n") "MODEL:\n"
      $ [ "gameStepNumber " <> show m.sys.gameStepNumber
        , "screenWidth " <> show m.sys.screenWidth
        , "screenHeight " <> show m.sys.screenHeight
        , "lastUpdateTime " <> show m.sys.lastUpdateTime
        , "actors " <> (intercalate ", " $ map show m.act.actors)
        , "recentlyAddedActors" <> show (map getNameId m.act.recentlyAddedActors)
        , "recentlyDeletedActors" <> show (map getNameId m.act.recentlyDeletedActors)
        , "gameState" <> show (m.game)
        , "currentUserInput" <> show (m.io.userInput)
        , "prevUserInput" <> show (m.io.prevUserInput)
        , "wsIn" <> show (m.io.wsIn)
        , "wsOut" <> show (m.io.wsOut)
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

getRandom :: forall ac gm x. Random x => AppMod ac gm x
getRandom = do
  (Model mr) <- get
  let randomPair = random mr.sys.seed :: RandomPair x
  modify_ (\(Model m) -> Model m{sys{seed = randomPair.newSeed}})
  pure randomPair.newVal

-- TODO setup Model with config
initialModelZeroTime :: forall ac gm. gm -> Model ac gm
initialModelZeroTime gameState =
  unsafePartial
    $ let
        Just time = instant (Milliseconds 0.0)
      in Model
        { 
          game : gameState
        , act : {
          actors : M.empty :: Map NameId (Actor ac)
        , recentlyAddedActors : []
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

mkNewNameId :: forall ac gm. AppMod ac gm NameId
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
