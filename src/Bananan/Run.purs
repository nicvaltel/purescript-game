module Bananan.Run(run) where

import Bananan.Reexport

import Bananan.Actors (mkActors, ActorData)
import Bananan.Control (ControlKey)
import Bananan.GameModel (ConfigState, GameConfig, GameModel, GameState)
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
            model <- liftEffect $ initGame config initialGameState mkActors
            when config.debugConfig $ liftEffect $ logShow config
            let
              rGame = runGame :: GameConfig -> GameStepFunc Int String ControlKey GameState ActorData -> GameModel -> Aff Unit
            rGame config gameStep model
