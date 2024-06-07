module Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import Bananan.Run as Bananan

main :: Effect Unit
main = do
    Bananan.run