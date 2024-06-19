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
      ballSpeed :: Number
    , ballDiameter :: Number
    , loseHeightLevel :: Number
    , initialRows :: Int
    , numberOfBallsInChainToDelete :: Int
    , nearestBallDiameterFactor :: Number -- used in findNearesBalls and findChainOfColor for search in range =  diameter * nearestBallDiameterFactor
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
