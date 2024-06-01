module Engine.Utils.Html
  ( getElementById
  , getElementCoordinatesById
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

foreign import _getElementById :: String -> HTMLDocument -> Effect (Nullable Element)
foreign import data NonElementParentNode :: Type

getElementById :: String -> HTMLDocument -> Effect (Maybe Element)
getElementById eid = map toMaybe <<< _getElementById eid

getElementCoordinatesById :: String -> Effect (Maybe { x :: Int, y :: Int })
getElementCoordinatesById elementId = do
  win <- window
  doc <- document win
  maybeElem <- getElementById elementId doc
  case maybeElem of
    Nothing -> pure Nothing
    Just elem -> do
      rect <- getBoundingClientRect elem
      pure $ Just { x: floor rect.left, y: floor rect.top }

-- import Data.Maybe (Maybe(..))
-- import Effect (Effect)
-- import Web.DOM.Document (toNonElementParentNode)
-- import Web.DOM.Element (setAttribute)
-- import Web.DOM.NonElementParentNode (getElementById)
-- import Web.HTML (window)
-- import Web.HTML.HTMLDocument (toDocument)
-- import Web.HTML.Window (document)

-- main :: Effect Unit
-- main = do
--   w ← window
--   d ← document w
--   maybeElement ← getElementById "test-input" $ toNonElementParentNode $ toDocument  d
--   case maybeElement of
--     Nothing → pure unit
--     Just elem → do
--       setAttribute "value" "new-value" elem