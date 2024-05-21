module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log,logShow)
import RunGame (runGame)
import Effect.Aff (launchAff_)
import ResourceLoader(fileLoader)
import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Core (stringify)
import Effect.Class (liftEffect)

main :: Effect Unit
main = launchAff_ $ do
    liftEffect $ log "🍝"

    conf <- fileLoader "assets/config.json"
    liftEffect $ logShow conf

    let env = jsonParser <$> conf
    liftEffect $  logShow $ (map stringify) <$> env
    
    runGame


