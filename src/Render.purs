module Render where

import Graphics.Canvas
import Prelude
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import GameModel (Model)
import Partial.Unsafe (unsafePartial)

square :: Int -> Int -> Int -> Rectangle
square size x y =
  { x: toNumber (size * x)
  , y: toNumber (size * y)
  , width: toNumber size
  , height: toNumber size
  }

render :: Model -> Effect Unit
render m =
  unsafePartial
    $ do
        log (show m)
        Just canvas <- getCanvasElementById "gameBoard"
        ctx <- getContext2D canvas
        canvasDim <- getCanvasDimensions canvas
        save ctx
        clearRect ctx $ { x: 0.0, y: 0.0, width: canvasDim.width, height: canvasDim.width }
        fillPath ctx
          $ rect ctx
              { x: 0.0
              , y: 0.0
              , width: canvasDim.width
              , height: canvasDim.height
              }
        clearRect ctx $ { x: 0.0, y: 0.0, width: 300.0, height: 200.0 }
        restore ctx
