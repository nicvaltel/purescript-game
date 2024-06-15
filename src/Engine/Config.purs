module Engine.Config where

import Engine.Reexport

type Config
  = { frameRateNumber :: Number
    , websocketUrl :: String
    , canvasElementId :: String
    , debugConfig :: Boolean
    , debugModel :: Boolean
    , debugWebsocket :: Boolean
    , debugUserInput :: Boolean
    , maxDeltaTime :: Number -- in millisesconds
    -- , state :: gm
    -- , actors :: Array 
    --     { 
    --       nameId :: String
    --     , x :: Number
    --     , y :: Number
    --     , z :: Int
    --     , cssClass :: String
    --     , imageSource :: String
    --     , data :: ac 
    --     }
    }

-- decodeJson :: DecodeJson a => Json -> Either JsonDecodeError a
-- https://github.com/purescript-contrib/purescript-argonaut-codecs/blob/main/docs/README.md
fromJson ::  
  Json -> 
  Either String Config
fromJson = mapLeft (\err -> "Cannot decode json config file: " <> show err) <<< decodeJson
