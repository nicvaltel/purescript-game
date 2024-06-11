module Main where

import Prelude

import Bananan.Run as Bananan
import Effect (Effect)
import Effect.Class.Console (log)
import PG.ModModRoutine as MM



main :: Effect Unit
main = do
    MM.run
    -- Bananan.run