module Engine.Config where

import Prelude
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Class (decodeJson)
import Data.Either (Either)
import Engine.Utils.Utils (mapLeft)

type Config
  = { frameRateNumber :: Number
    , websocketUrl :: String
    , canvasElementId :: String
    , debugConfig :: Boolean
    , debugModel :: Boolean
    , debugWebsocket :: Boolean
    , debugUserInput :: Boolean
    , images :: Array { name :: String, path :: String }
    }

-- decodeJson :: DecodeJson a => Json -> Either JsonDecodeError a
-- https://github.com/purescript-contrib/purescript-argonaut-codecs/blob/main/docs/README.md
fromJson :: Json -> Either String Config
fromJson = mapLeft (\err -> "Cannot decode json config file: " <> show err) <<< decodeJson
