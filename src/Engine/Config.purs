module Engine.Config where

import Engine.Reexport

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
        , cssClass :: String
        , imageSource :: String
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
