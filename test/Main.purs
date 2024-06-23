module Test.Main where

import Bananan.Actors
import Prelude

import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Effect (Effect)
import Effect.Class.Console (log)
import Data.Either

main :: Effect Unit
main = do
  log "🍝"
  log (show testJsonBallColor)

testJsonBallColor :: Array Boolean
testJsonBallColor = 
  [ encodeDecode Red
  , encodeDecode Green
  , encodeDecode Blue
  , encodeDecode Yellow
  , encodeDecode Purple
  ]


encodeDecode :: forall a. Eq a => EncodeJson a => DecodeJson a => a -> Boolean
encodeDecode a = decodeJson (encodeJson a) == Right a  

