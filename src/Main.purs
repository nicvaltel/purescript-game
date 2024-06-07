module Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)

import Bananan.Run as Run

main :: Effect Unit
main = do
    log "OKAY"
    pure unit
    Run.run