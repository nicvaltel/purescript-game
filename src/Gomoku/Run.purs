module Gomoku.Run(run) where

import Prelude

import Gomoku.Control (ControlKey)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (launchAff_, Aff)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Engine.GameLoop (GameStepFunc, runGame)
import Engine.ResourceLoader (parseConfigFile)
import Gomoku.GameModel (ActorState, GameConfig, GameModel, GameState, ConfigState)
import Gomoku.GameStep (gameStep)
import Engine.InitGame (initGame)

configFilePath ∷ String
configFilePath = "config.json"

initialGameState :: GameState
initialGameState =
  { gridSize: 0
  }

run :: Effect Unit
run =
  launchAff_
    $ do
        eitherConf <- parseConfigFile configFilePath
        case eitherConf of
          Left err -> liftEffect $ log $ "Error in config file " <> configFilePath <> ":\n" <> err
          Right config -> do
            model <- liftEffect $ initGame config initialGameState
            when config.debugConfig $ liftEffect $ logShow config
            let
              rGame = runGame :: GameConfig -> GameStepFunc ControlKey GameState ActorState ConfigState -> GameModel -> Aff Unit
            rGame config gameStep model
