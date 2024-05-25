module Render where

import Graphics.Canvas
import Prelude

import Config (Config)
import Data.Int (toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Console (log)
import GameModel (Model, showModel)
import Partial.Unsafe (unsafePartial)
import Utils.Utils (undefined)
import Data.Traversable(for)
import Data.Number(floor)

square :: Int -> Int -> Int -> Rectangle
square size x y =
  { x: toNumber (size * x)
  , y: toNumber (size * y)
  , width: toNumber size
  , height: toNumber size
  }

render :: Config -> Model -> Effect Unit
render conf m =
  unsafePartial
    $ do
        when conf.debug $ log (showModel m)
        Just canvas <- getCanvasElementById "canvas"
        ctx <- getContext2D canvas
        canvasDim <- getCanvasDimensions canvas
        save ctx
        clearRect ctx $ { x: 0.0, y: 0.0, width: canvasDim.width, height: canvasDim.width }
        -- fillPath ctx
        --   $ rect ctx
        --       { x: 0.0
        --       , y: 0.0
        --       , width: canvasDim.width
        --       , height: canvasDim.height
        --       }
        -- clearRect ctx $ { x: 0.0, y: 0.0, width: 300.0, height: 200.0 }

        -- drawImage :: Context2D -> CanvasImageSource -> Number -> Number -> Effect Unit
        _ <- for m.actors $ \actor -> do
            let sprite = fromMaybe undefined (Map.lookup actor.spriteName m.sprites)
            drawImage ctx sprite (floor actor.x) (floor actor.y)


        -- let redBall = fromMaybe undefined (Map.lookup "red_ball" m.sprites)
        -- drawImage ctx redBall (toNumber m.gameStepNumber) (toNumber m.gameStepNumber)

        restore ctx
