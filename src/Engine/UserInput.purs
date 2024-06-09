module Engine.UserInput
  ( UserInput(..)
  , emptyUserInput
  , getUserInput
  )
  where

import Prelude
import Effect (Effect)
import Engine.Types (MouseCoodr)
import Web.HTML (HTMLElement)


foreign import _getElementOffset :: HTMLElement -> Effect MouseCoodr
foreign import _getPressedKeys :: Effect (Array String)
foreign import _getMousePressedButtons :: Effect (Array String)
foreign import _getMouseCoordinates :: Effect MouseCoodr

-- class
--   (Bounded ui, Enum ui, Show ui) <= Control ui where
--   controlKeyMap :: ui -> Int

type UserInput
  = { keys :: Array String
    , mousePos :: MouseCoodr
    , mouseRelativePos :: MouseCoodr
    , mouseBtns :: Array String
    }

emptyUserInput :: UserInput
emptyUserInput = 
    { keys : []
    , mousePos :{ x: 0.0, y : 0.0 }
    , mouseRelativePos : {x:0.0, y: 0.0}
    , mouseBtns : []
    }

getUserInput :: HTMLElement -> Effect UserInput
getUserInput canvasElem = do
  keys <- _getPressedKeys
  mouseBtns <- _getMousePressedButtons
  mousePos <- _getMouseCoordinates
  canvasPos <- _getElementOffset canvasElem
  pure
    { keys : keys
    , mousePos : mousePos
    , mouseRelativePos : { x: mousePos.x - canvasPos.x, y: mousePos.y - canvasPos.y }
    , mouseBtns : mouseBtns
    }