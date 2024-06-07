module Bananan.Run(run) where

import Bananan.Reexport

import Bananan.Actors (mkActorData, ActorData)
import Bananan.Control (ControlKey)
import Bananan.GameModel (ConfigState, GameConfig, GameModel, GameState)
import Bananan.GameStep (gameStep)
import Engine.Config (Config)
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
        eitherConf <- parseConfigFile configFilePath :: Aff ( Either String (Config ActorData GameState))
        liftEffect $ logShow eitherConf
        case eitherConf of
          Left err -> liftEffect $ log $ "Error in config file " <> configFilePath <> ":\n" <> err
          Right config -> do
            model <- liftEffect $ initGame config initialGameState mkActorData
            when config.debugConfig $ liftEffect $ logShow config
            let
              rGame = runGame :: GameConfig -> GameStepFunc ActorData GameState ControlKey GameState ActorData -> GameModel -> Aff Unit
            rGame config gameStep model

