module PG.GetInput where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)

newtype RequestAnimationFrameId = RequestAnimationFrameId Int

foreign import _requestAnimationFrame :: Effect Unit -> Effect RequestAnimationFrameId
foreign import _getPressedKeys :: Effect (Array String)
foreign import _getMousePressedButtons :: Effect (Array String)
foreign import _getMouseCoordinates :: Effect { x :: Number, y :: Number }

getPressedKeysExample :: Effect Unit
getPressedKeysExample = do
    keys <- _getPressedKeys
    log $ "Pressed keys: " <> (show keys)

getMousePressedButtonsExample :: Effect Unit
getMousePressedButtonsExample = do
    buttons <- _getMousePressedButtons
    log $ "Pressed mouse buttons: " <> (show buttons)

getMouseCoordinatesExample :: Effect Unit
getMouseCoordinatesExample = do
    coords <- _getMouseCoordinates
    log $ "Mouse coordinates: " <> "x: " <> (show coords.x) <> ", y: " <> (show coords.y)


loop ∷ Effect Unit
loop = do
    getPressedKeysExample
    getMousePressedButtonsExample
    getMouseCoordinatesExample
    _ <- _requestAnimationFrame loop
    pure unit
