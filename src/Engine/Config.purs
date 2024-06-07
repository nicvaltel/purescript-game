module Engine.Config where

import Prelude
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Class (class DecodeJsonField, decodeJson)
import Data.Either (Either)
import Engine.Utils.Utils (mapLeft)

type Config ac gm
  = { frameRateNumber :: Number
    , websocketUrl :: String
    , canvasElementId :: String
    , debugConfig :: Boolean
    , debugModel :: Boolean
    , debugWebsocket :: Boolean
    , debugUserInput :: Boolean
    , state :: gm
    , actors :: Array 
        { 
          nameId :: String
        , x :: Number
        , y :: Number
        , z :: Int
        , data :: ac 
        }
    }

-- decodeJson :: DecodeJson a => Json -> Either JsonDecodeError a
-- https://github.com/purescript-contrib/purescript-argonaut-codecs/blob/main/docs/README.md
fromJson :: forall ac gm. 
  DecodeJsonField ac => 
  DecodeJsonField gm => 
  Json -> 
  Either String (Config ac gm)
fromJson = mapLeft (\err -> "Cannot decode json config file: " <> show err) <<< decodeJson
