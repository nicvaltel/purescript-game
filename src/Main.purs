module Main where

import Prelude

import Control (ControlKey)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (launchAff_, Aff)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Engine.GameLoop (GameStepFunc, runGame)
import Engine.ResourceLoader (parseConfigFile)
import GameModel (ActorState, GameConfig, GameModel, GameState, ConfigState)
import GameStep (gameStep)
import InitGame (initGame)

configFilePath ∷ String
configFilePath = "config.json"

main :: Effect Unit
main =
  launchAff_
    $ do
        eitherConf <- parseConfigFile configFilePath
        case eitherConf of
          Left err -> liftEffect $ log $ "Error in config file " <> configFilePath <> ":\n" <> err
          Right config -> do
            model <- liftEffect $ initGame config
            when config.debugConfig $ liftEffect $ logShow config
            let
              rGame = runGame :: GameConfig -> GameStepFunc ControlKey GameState ActorState ConfigState -> GameModel -> Aff Unit
            rGame config gameStep model
