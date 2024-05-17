module Constants where

import Prelude

type Constants
  = { frameRateNumber :: Number
    , websocketMessageBufferSize :: Int
    , websocketUrl :: String
    }

constants :: Constants
constants =
  { -- frameRateNumber: 20.0 -- in milliseconds
  frameRateNumber: 2_000.0 -- in milliseconds
  , websocketMessageBufferSize: 32
  , websocketUrl: "ws://95.140.155.123:1234/ws"
  }
