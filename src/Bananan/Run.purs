module Bananan.Run(run) where

import Bananan.Reexport

import Bananan.Actors (ActorData(..), ballQueueActorMock, colorFromRandomInt, dragonMock)
import Bananan.GameConfig (GameConfig, gameConfigFromJson)
import Bananan.GameModel (GameState(..), mkActorData)
import Bananan.GameStep (gameStep)
import Control.Monad.State (runStateT)
import Data.Either (either)
import Data.Map as M
import Engine.Config (Config)
import Engine.GameLoop (GameStepFunc, runGame)
import Engine.InitGame (initGame)
import Engine.Model (Actor(..), AppModAff, actorMock, initialModelZeroTime, mkUniqueNameId)
import Engine.Random.PseudoRandom (randomEff)
import Engine.ResourceLoader (getHtmlElement, loadJson, parseConfigFile)
import Engine.Utils.Html (getElementById)
import Web.DOM.Element (getBoundingClientRect)

-- import Web.HTML.HTMLCanvasElement (width)

configFilePath :: FilePath

configFilePath = "config.json"

gameConfigFilePath :: FilePath
gameConfigFilePath = "game_config.json"


initialGameState :: Config -> GameConfig -> Effect GameState
initialGameState conf gameConf = do
  n :: Int <- randomEff
  mbCanvas <- getElementById conf.canvasElementId
  rect <- case mbCanvas of
    Just canvas -> getBoundingClientRect canvas
    Nothing -> error $ "Canvas not found. canvasId = " <> conf.canvasElementId
  mbElem <- getHtmlElement "gun"
  let nameGun = mkUniqueNameId "gun"
  let gunData = {
                angleSpeed: 0.0,
                maxAngleSpeed: 0.2,
                maxLeftAngle: -80.0,
                maxRightAngle: 80.0
              }
  let gun = Actor
        {
            nameId: nameGun,
            x: 287.5,
            y: 810.0,
            z: 1,
            width : 25.0,
            height : 74.0,
            visible : true,
            angle : 0.0,
            htmlElement : mbElem,
            cssClass : "gun",
            imageSource: "../images/gun.png",
            data: ActorGun gunData
            }
  pure $ GameState -- TODO fill with config
    {
      score: 0
    , ballQueue : 
        {
          color : colorFromRandomInt n
        , flying : Nothing
        } 
    , canvasWidth : rect.width
    , gunNameId : mkUniqueNameId "gun"
    , ballSpeed : 0.8
    , actors : 
          { balls : M.empty
          , gun : gun
          , dragon : let (Actor a) = actorMock in Actor a{data = ActorDragon dragonMock}
          , ballQueueActor : let (Actor a) = actorMock in Actor a{data = ActorBallQueue ballQueueActorMock}
          } 
    }

run :: Effect Unit
run =
  launchAff_
    $ do
        eitherConf <- parseConfigFile configFilePath :: Aff ( Either String Config)
        let config = either (\err -> error $ "Error in config file " <> configFilePath <> ":\n" <> err) identity eitherConf
        
        eitherGameConf <- parseGameConfigFile gameConfigFilePath :: Aff ( Either String GameConfig)
        let gameConfig = either (\err -> error $ "Error in gameConfig file " <> gameConfigFilePath <> ":\n" <> err) identity eitherGameConf

        when (config.debugConfig) $ liftEffect do 
           logShow config
           logShow gameConfig

        gameState <- liftEffect $ initialGameState config gameConfig
        _ <- runStateT (runAppModEff config gameConfig gameState) (initialModelZeroTime gameState)
        pure unit


runAppModEff :: Config -> GameConfig -> GameState -> AppModAff ActorData GameState Unit
runAppModEff config gameConfig gameState = do
  initGame config gameState mkActorData -- TODO pass initGame thrue runStateT
  let rGame = runGame :: Config -> GameStepFunc ActorData GameState -> AppModAff ActorData GameState Unit
  rGame config gameStep

parseGameConfigFile ::  
  FilePath -> 
  Aff (Either String GameConfig)
parseGameConfigFile filePath = do 
  eitherJson <- loadJson filePath
  pure $ join (gameConfigFromJson <$> eitherJson)