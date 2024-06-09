module Bananan.Control
  ( ControlKey(..)
  ) where

import Bananan.Reexport
import Data.Bounded as B
import Data.Enum as E
import Data.Maybe (Maybe(..))

data ControlKey
  = Space -- 0x5e
  | ArrowLeft -- 0x61
  | ArrowRight -- 0x63

instance showControlKey :: Show ControlKey where
  show Space = " "
  show ArrowLeft = "ArrowLeft"
  show ArrowRight = "ArrowRight"

derive instance eqControlKey :: Eq ControlKey

derive instance ordControlKey :: Ord ControlKey

instance readControlKey :: Read ControlKey where
    read = inverseMap show
    -- read " " = Just Space
    -- read "ArrowLeft" = Just ArrowLeft
    -- read "ArrowRight" = Just ArrowRight
    -- read _ = Nothing

instance enumControlKey :: E.Enum ControlKey where
  succ Space = Just ArrowLeft
  succ ArrowLeft = Just ArrowRight
  succ ArrowRight = Nothing
  pred Space = Nothing
  pred ArrowLeft = Just Space
  pred ArrowRight = Just ArrowLeft

instance boundedControlKey :: B.Bounded ControlKey where
  bottom = Space
  top = ArrowRight
