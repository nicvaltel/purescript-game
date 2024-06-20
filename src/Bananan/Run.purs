module Bananan.Run(run) where

import Bananan.Reexport

import Bananan.Actors (ActorData(..), ballQueueActorMock, colorFromRandomInt, dragonMock)
import Bananan.GameConfig (GameConfig, gameConfigFromJson, selectBallQueueImageSource)
import Bananan.GameModel (AppGame, GameState(..), getGameRec, mkActorData)
import Bananan.GameStep (addRandomBalls, gameStep)
import Control.Monad.State (evalStateT, runStateT)
import Data.Either (either)
import Data.Foldable (for_)
import Data.Int (even, odd)
import Data.List as List
import Data.Map as M
import Engine.Config (Config)
import Engine.GameLoop (GameStepFunc, runGame)
import Engine.Model (Actor(..), AppModAff, actorMock, appModToAppModAff, initialModelZeroTime, mkUniqueNameId)
import Engine.Random.PseudoRandom (randomEff)
import Engine.ResourceLoader (getHtmlElement, loadJson, parseConfigFile)
import Engine.Utils.Html (getElementById)
import Web.DOM.Element (getBoundingClientRect)

configFilePath :: FilePath

configFilePath = "config.json"

gameConfigFilePath :: FilePath
gameConfigFilePath = "game_config.json"


initialGameState :: Config -> GameConfig -> Effect GameState
initialGameState conf gameConf = do
  n :: Int <- randomEff
  currentTime <- now
  let randColor =  colorFromRandomInt n 
  mbCanvas <- getElementById conf.canvasElementId
  rect <- case mbCanvas of
    Just canvas -> getBoundingClientRect canvas
    Nothing -> error $ "Canvas not found. canvasId = " <> conf.canvasElementId

  let gunConf = gameConf.actors.gun
  mbElemGun <- getHtmlElement gunConf.nameId
  let ballQ = gameConf.actors.ballQueueActor
  mbElemBallQ <- getHtmlElement ballQ.nameId
  let gunConfData = case gameConf.actors.gun.data of
        ActorGun d -> d
        _ -> error "gameConfig.actors.gun.data is not ActorGun"
  let gun = Actor
        {
          nameId: mkUniqueNameId gunConf.nameId
        ,  x: gunConf.x
        ,  y: gunConf.y
        ,  z: gunConf.z
        ,  width : gunConf.width
        ,  height : gunConf.height
        ,  visible : true
        ,  angle : 0.0
        ,  htmlElement : mbElemGun
        ,  cssClass : gunConf.cssClass
        ,  imageSource: gunConf.imageSource
        ,  data: ActorGun 
                { angleSpeed: gunConfData.angleSpeed
                , maxAngleSpeed: gunConfData.maxAngleSpeed
                , maxLeftAngle: gunConfData.maxLeftAngle
                , maxRightAngle: gunConfData.maxRightAngle
                }
            }

  let ballQueue = Actor
        {
          nameId: mkUniqueNameId ballQ.nameId
        ,  x: ballQ.x
        ,  y: ballQ.y
        ,  z: ballQ.z
        ,  width : ballQ.width
        ,  height : ballQ.height
        ,  visible : true
        ,  angle : 0.0
        ,  htmlElement : mbElemBallQ
        ,  cssClass : ballQ.cssClass
        ,  imageSource: selectBallQueueImageSource gameConf randColor
        ,  data: ActorBallQueue ballQueueActorMock
        }
  
  pure $ GameState 
    {
      score: 0
    , ballQueue : { color : randColor } 
    , canvasWidth : rect.width
    , canvasHeight : rect.height
    , ballSpeed : gameConf.ballSpeed
    , gameIsRunning : true
    , shotsCounter : 0
    , lastRowsAdded : { time : currentTime, numberOfBalls : if odd gameConf.initialRows then 8 else 7 }
    , actors : 
          { balls : M.empty
          , flyingBall : Nothing
          , gun : gun
          , dragon : let (Actor a) = actorMock in Actor a{data = ActorDragon dragonMock} -- TODO fill with config
          , ballQueueActor : ballQueue
          }
    , graphBall : List.Nil
    }

initialBallRows :: GameConfig -> AppGame Unit
initialBallRows gameConf = do
  game <- getGameRec <$> get
  let deltaH = gameConf.ballDiameter * 0.866 -- 0.866 = sqrt(3)/2
  for_ (range 0 (gameConf.initialRows - 1)) $ \r -> do
    let n = if even r then gameConf.ballsInSmallRow + 1 else gameConf.ballsInSmallRow
    addRandomBalls gameConf n game.canvasWidth (toNumber r * deltaH)

run :: Effect Unit
run = do
  seed :: Int <- randomEff
  launchAff_
    $ do
        eitherConf <- parseConfigFile configFilePath :: Aff ( Either String Config)
        let config = either (\err -> error $ "Error in config file " <> configFilePath <> ":\n" <> err) identity eitherConf
        
        eitherGameConf <- parseGameConfigFile gameConfigFilePath :: Aff ( Either String GameConfig)
        let gameConfig = either (\err -> error $ "Error in gameConfig file " <> gameConfigFilePath <> ":\n" <> err) identity eitherGameConf

        when (config.debugConfig) $ liftEffect do 
           logShow config
           logShow gameConfig

        gameState@(GameState gs) <- liftEffect $ initialGameState config gameConfig
        evalStateT (runAppModEff config gameConfig) (initialModelZeroTime seed gs.canvasWidth gs.canvasHeight gameState)


runAppModEff :: Config -> GameConfig -> AppModAff ActorData GameState Unit
runAppModEff config gameConfig = do
  appModToAppModAff $ initialBallRows gameConfig
  let rGame = runGame :: Config -> GameStepFunc ActorData GameState -> AppModAff ActorData GameState Unit
  rGame config (gameStep gameConfig)

parseGameConfigFile ::  
  FilePath -> 
  Aff (Either String GameConfig)
parseGameConfigFile filePath = do 
  eitherJson <- loadJson filePath
  pure $ join (gameConfigFromJson <$> eitherJson)