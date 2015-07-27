module Test.Main where

import Prelude

import Control.Events (EVENT(), Event(..))
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console (CONSOLE(), log)
import Data.Foreign (tagOf, toForeign)

import Node.Events (emit, emitter, on, newListenerEvent)

toString :: forall a. a -> String
toString = tagOf <<< toForeign

log' :: forall a eff. String -> a -> Eff (console :: CONSOLE | eff) Unit
log' s = log <<< append s <<< toString

main :: Eff (event :: EVENT, console :: CONSOLE) Boolean
main = emitter
  >>= on newListenerEvent (log' "added a new event: ")
  >>= on (Event "Here's some candy") (log' "Check out this candy: ")
  >>= emit (Event "Here's some candy") "Skittles"
