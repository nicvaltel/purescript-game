module Bananan.GameConfig where

import Engine.Reexport

import Bananan.Actors (ActorData, BallColor)

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
    , addNewRowsTimeInterval :: Number -- im milliseconds
    , ballsInSmallRow :: Int  -- ballsInBigRow = ballsInSmallRow + 1
    , actors :: 
        {
          gun :: {data :: ActorData | ActorCommonConfig }
        , dragon :: {data :: ActorData | ActorCommonConfig }
        , ballQueue :: {data :: ActorData | ActorCommonConfig }
        }
    , colorImageSources :: Array {color :: String, imageSource :: FilePath}
    , audio :: { shoot :: FilePath  }
    }


gameConfigFromJson ::  
  Json -> 
  Either String GameConfig
gameConfigFromJson = mapLeft (\err -> "Cannot decode json config file: " <> show err) <<< decodeJson


selectBallQueueImageSource :: GameConfig -> BallColor -> FilePath
selectBallQueueImageSource gameConf ballColor = do 
  let ballColorStr = show ballColor
  let filteredSources = filter (\{color} -> color == ballColorStr) gameConf.colorImageSources 
  case head filteredSources of
    Just {imageSource} -> imageSource
    _ -> error $ "Not BallColor in GameConfig.colorImageSources color = " <> ballColorStr 