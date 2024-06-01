module Engine.Render.Render2JS (render) where

import Graphics.Canvas (getCanvasElementById, getContext2D, restore, save, getCanvasDimensions, clearRect)
import Prelude
import Engine.Config (Config)
import Data.Maybe (Maybe(..))
import Data.Number (floor)
import Data.Traversable (for)
import Effect (Effect)
import Effect.Console (log)
import Engine.Model (Model, showModel)
import Partial.Unsafe (unsafePartial)

data Img

foreign import _drawJupiter :: Img -> Number -> Number -> Effect Unit

-- foreign import _getElementById :: String -> Effect String
foreign import _getElementById :: String -> Effect Img

foreign import _setElementById :: String -> String -> Effect Unit

render :: Config -> Model -> Effect Unit
render conf m =
  unsafePartial
    $ do
        when conf.debugModel $ log (showModel m)
        Just canvas <- getCanvasElementById "canvas"
        ctx <- getContext2D canvas
        canvasDim <- getCanvasDimensions canvas
        jupiter <- _getElementById "jupiter"
        save ctx
        clearRect ctx $ { x: 0.0, y: 0.0, width: canvasDim.width, height: canvasDim.width }
        _ <-
          for m.actors
            $ \actor -> do
                _drawJupiter jupiter (floor actor.x) (floor actor.y)
        restore ctx
