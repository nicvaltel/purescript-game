module Bananan.Run(run) where

import Bananan.Reexport

import Bananan.Control (ControlKey)
import Bananan.GameModel (ConfigState, GameConfig, GameModel, GameState, GameActor)
import Bananan.GameStep (gameStep)
import Engine.GameLoop (GameStepFunc, runGame)
import Engine.InitGame (initGame)
import Engine.ResourceLoader (parseConfigFile)

configFilePath ∷ String
configFilePath = "config.json"

initialGameState :: GameState
initialGameState = {score: 0}

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
              rGame = runGame :: GameConfig -> GameStepFunc ControlKey GameState GameActor -> GameModel -> Aff Unit
            rGame config gameStep model
