module Bananan.GameConfig where

import Prelude

import Bananan.Actors (Dragon, Gun, BallQueueActor)

type ActorCommonConfig = 
    ( nameId :: String
    , x :: Number
    , y :: Number
    , z :: Int
    , cssClass :: String
    , imageSource :: String
    )


type GameConfig
  = { 
      score :: Int
    , ballSpeed :: Number
    , actors :: 
        {
          gun :: {data :: Gun | ActorCommonConfig }
        , dragon :: {data :: Dragon | ActorCommonConfig }
        , ballQueueActor :: {data :: BallQueueActor | ActorCommonConfig }
        }
    }