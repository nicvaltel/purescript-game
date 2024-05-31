module Main where

import Prelude
import Control (ControlKey)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (launchAff_, Aff)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Engine.Config (Config)
import Engine.GameLoop (GameStepFunc, runGame)
import Engine.Model (Model)
import Engine.ResourceLoader (parseConfigFile)
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
            let
              model = initGame config
            when config.debug $ liftEffect $ logShow config
            let
              rGame = runGame :: Config -> GameStepFunc ControlKey -> Model -> Aff Unit
            rGame config gameStep model
