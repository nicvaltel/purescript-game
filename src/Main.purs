module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import RunGame (runGame)

main :: Effect Unit
main = do
  log "🍝"
  runGame


