module Engine.Utils.Html
  ( getElementById
  , getElementCoordinatesById
  , getNodeElementById
  )
  where

import Prelude

import Data.Int (floor)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Web.DOM.Element (Element, getBoundingClientRect)
import Web.HTML (HTMLDocument, window)
import Web.HTML.Window (document)

foreign import _getNodeElementById :: String -> HTMLDocument -> Effect (Nullable Element)
foreign import data NonElementParentNode :: Type

getNodeElementById :: String -> HTMLDocument -> Effect (Maybe Element)
getNodeElementById eid = map toMaybe <<< _getNodeElementById eid

getElementById :: String -> Effect (Maybe Element)
getElementById eid = do
  win <- window
  doc <- document win
  getNodeElementById eid doc

getElementCoordinatesById :: String -> Effect (Maybe { x :: Int, y :: Int })
getElementCoordinatesById elementId = do
  maybeElem <- getElementById elementId
  case maybeElem of
    Nothing -> pure Nothing
    Just elem -> do
      rect <- getBoundingClientRect elem
      pure $ Just { x: floor rect.left, y: floor rect.top }
