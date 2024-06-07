module Engine.Config where

import Prelude
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Class (class DecodeJsonField, decodeJson)
import Data.Either (Either)
import Engine.Utils.Utils (mapLeft)

type Config cfgac cfgst
  = { frameRateNumber :: Number
    , websocketUrl :: String
    , canvasElementId :: String
    , debugConfig :: Boolean
    , debugModel :: Boolean
    , debugWebsocket :: Boolean
    , debugUserInput :: Boolean
    , state :: cfgst
    , actors :: Array 
        { 
          nameId :: String
        , x :: Number
        , y :: Number
        , z :: Int
        , data :: cfgac 
        }
    }

-- decodeJson :: DecodeJson a => Json -> Either JsonDecodeError a
-- https://github.com/purescript-contrib/purescript-argonaut-codecs/blob/main/docs/README.md

-- fromJson :: forall cfg ac. DecodeJsonField cfg => DecodeJsonField ac => Json -> Either String (Config cfg ac)
fromJson :: forall cfgac cfgst. 
  DecodeJsonField cfgac => 
  DecodeJsonField cfgst => 
  Json -> 
  Either String (Config cfgac cfgst)
fromJson = mapLeft (\err -> "Cannot decode json config file: " <> show err) <<< decodeJson
