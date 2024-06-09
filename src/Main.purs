module Main where

import Prelude

import Bananan.Run as Bananan
import Effect (Effect)
import Effect.Class.Console (log)
import PG.GetInput as GetInput



main :: Effect Unit
main = do
    -- GetInput.loop
    Bananan.run