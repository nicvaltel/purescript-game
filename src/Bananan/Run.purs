module Bananan.Run(run) where

import Bananan.Reexport

import Bananan.Actors (ActorData(..), ballQueueActorMock, colorFromRandomInt, dragonMock)
import Bananan.GameModel (GameConfig, GameState(..), mkActorData)
import Bananan.GameStep (gameStep)
import Control.Monad.State (runStateT)
import Data.Map as M
import Engine.Config (Config)
import Engine.GameLoop (GameStepFunc, runGame)
import Engine.InitGame (initGame)
import Engine.Model (Actor(..), AppModAff, actorMock, initialModelZeroTime, mkUniqueNameId)
import Engine.Random.PseudoRandom (randomEff)
import Engine.ResourceLoader (getHtmlElement, parseConfigFile)
import Engine.Utils.Html (getElementById)
import Web.DOM.Element (getBoundingClientRect)
-- import Web.HTML.HTMLCanvasElement (width)

configFilePath ∷ String
configFilePath = "config.json"



initialGameState :: GameConfig -> Effect GameState
initialGameState conf = do
  n :: Int <- randomEff
  mbCanvas <- getElementById conf.canvasElementId
  rect <- case mbCanvas of
    Just canvas -> getBoundingClientRect canvas
    Nothing -> error $ "Canvas not found. canvasId = " <> conf.canvasElementId
  let (GameState confState) = conf.state
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
    , gunNameId : confState.gunNameId -- mkUniqueNameId "gun"
    , ballSpeed : confState.ballSpeed
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
        eitherConf <- parseConfigFile configFilePath :: Aff ( Either String (Config ActorData GameState))
        liftEffect $ logShow eitherConf
        case eitherConf of
          Left err -> liftEffect $ log $ "Error in config file " <> configFilePath <> ":\n" <> err
          Right config -> do
            gameState <- liftEffect $ initialGameState config
            _ <- runStateT (runAppModEff config gameState) (initialModelZeroTime gameState)
            pure unit


runAppModEff :: GameConfig -> GameState -> AppModAff GameState Unit
runAppModEff config gameState = do
  initGame config gameState mkActorData -- TODO pass initGame thrue runStateT
  when config.debugConfig $ liftEffect $ logShow config
  let rGame = runGame :: GameConfig -> GameStepFunc ActorData GameState -> AppModAff GameState Unit
  rGame config gameStep