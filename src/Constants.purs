module Constants where

import Prelude

type Constants
  = { frameRateNumber :: Number
    , websocketUrl :: String
    }

constants :: Constants
constants =
  { 
    frameRateNumber: 20.0 -- in milliseconds
  , websocketUrl: "ws://95.140.155.123:1234/ws"
  }
