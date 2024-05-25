module Main where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log,logShow)
import ResourceLoader (parseConfigFile)
import RunGame (runGame)

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
            when config.debug $ liftEffect $ logShow config
            runGame config
