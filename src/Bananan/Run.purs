module Bananan.Run(run) where

import Bananan.Reexport

import Bananan.Actors (ActorData, colorFromRandomInt)
import Bananan.GameModel (GameConfig, GameModel, GameState, mkActorData)
import Bananan.GameStep (gameStep)
import Control.Monad.State (runStateT)
import Control.Monad.Trans.Class (lift)
import Engine.Config (Config)
import Engine.GameLoop (GameStepFunc, runGame)
import Engine.InitGame (initGame)
import Engine.Model (AppModAff, AppModEffect, initialModelZeroTime)
import Engine.Random.PseudoRandom (randomEff)
import Engine.ResourceLoader (parseConfigFile)

configFilePath ∷ String
configFilePath = "config.json"

initialGameState :: Effect GameState
initialGameState = do
  n :: Int <- randomEff
  pure
    {
      score: 0
    , ballQueue : 
        {
          color : colorFromRandomInt n
        , flying : Nothing
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
            gameState <- liftEffect initialGameState
            _ <- runStateT (runAppModEff config gameState) (initialModelZeroTime gameState)
            pure unit


runAppModEff :: GameConfig -> GameState -> AppModAff ActorData GameState Unit
runAppModEff config gameState = do
  initGame config gameState mkActorData -- TODO pass initGame thrue runStateT
  when config.debugConfig $ liftEffect $ logShow config
  let rGame = runGame :: GameConfig -> GameStepFunc ActorData GameState -> AppModAff ActorData GameState Unit
  rGame config gameStep