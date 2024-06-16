module Bananan.GameConfig where

import Engine.Reexport

import Bananan.Actors (ActorData)

type ActorCommonConfig = 
    ( nameId :: String
    , x :: Number
    , y :: Number
    , width :: Number
    , height :: Number
    , z :: Int
    , cssClass :: String
    , imageSource :: String
    )


type GameConfig
  = { 
      score :: Int
    , ballSpeed :: Number
    , ballDiameter :: Number
    , actors :: 
        {
          gun :: {data :: ActorData | ActorCommonConfig }
        , dragon :: {data :: ActorData | ActorCommonConfig }
        , ballQueueActor :: {data :: ActorData | ActorCommonConfig }
        }
    }


gameConfigFromJson ::  
  Json -> 
  Either String GameConfig
gameConfigFromJson = mapLeft (\err -> "Cannot decode json config file: " <> show err) <<< decodeJson
