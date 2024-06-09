module Bananan.Control where
--   ( ControlKey(..)
--   ) where

-- import Prelude
-- import Data.Bounded as B
-- import Data.Enum as E
-- import Data.Maybe (Maybe(..))
-- import Engine.UserInput as UI

-- data ControlKey
--   = Space
--   | Left
--   | Up
--   | Right
--   | Down

-- instance showControlKey :: Show ControlKey where
--   show Space = "Space"
--   show Left = "Left"
--   show Up = "Up"
--   show Right = "Right"
--   show Down = "Down"

-- derive instance eqControlKey :: Eq ControlKey

-- derive instance ordControlKey :: Ord ControlKey

-- instance enumControlKey :: E.Enum ControlKey where
--   succ Space = Just Left
--   succ Left = Just Up
--   succ Up = Just Right
--   succ Right = Just Down
--   succ Down = Nothing
--   pred Space = Nothing
--   pred Left = Just Space
--   pred Up = Just Left
--   pred Right = Just Up
--   pred Down = Just Right

-- instance boundedControlKey :: B.Bounded ControlKey where
--   bottom = Space
--   top = Down

-- instance controlControlKey :: UI.Control ControlKey where
--   controlKeyMap :: ControlKey -> Int
--   controlKeyMap Space = 32
--   controlKeyMap Left = 37
--   controlKeyMap Up = 38
--   controlKeyMap Right = 39
--   controlKeyMap Down = 40
