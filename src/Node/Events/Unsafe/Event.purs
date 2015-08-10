module Node.Events.Unsafe.Event (Event(..)) where

import Prelude

newtype Event args = Event String

-- Instances ------------------------------------------------------------------
instance eqEvent :: Eq (Event args) where
  eq (Event a) (Event b) = a == b

instance showEvent :: Show (Event args) where
  show (Event name) = name
