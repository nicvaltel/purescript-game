module Bananan.Run(run) where

import Bananan.Reexport

import Bananan.Actors (ActorData, colorFromRandomInt)
import Bananan.GameModel (GameConfig, GameState(..), GameStateRec, mkActorData)
import Bananan.GameStep (gameStep)
import Control.Monad.State (runStateT)
import Engine.Config (Config)
import Engine.GameLoop (GameStepFunc, runGame)
import Engine.InitGame (initGame)
import Engine.Model (AppModAff, initialModelZeroTime)
import Engine.Random.PseudoRandom (randomEff)
import Engine.ResourceLoader (parseConfigFile)
import Engine.Utils.Html (getElementById)
import Web.DOM.Element (getBoundingClientRect)

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
  pure $ GameState
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


runAppModEff :: GameConfig -> GameState -> AppModAff ActorData GameState Unit
runAppModEff config gameState = do
  initGame config gameState mkActorData -- TODO pass initGame thrue runStateT
  when config.debugConfig $ liftEffect $ logShow config
  let rGame = runGame :: GameConfig -> GameStepFunc ActorData GameState -> AppModAff ActorData GameState Unit
  rGame config gameStep