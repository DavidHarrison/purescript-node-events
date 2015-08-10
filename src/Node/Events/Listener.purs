module Node.Events.Listener (Listener(), mkListener) where

import Prelude

import Control.Monad.Eff          (Eff())
import Data.Function.Multivariate (Fn())

import Node.Events.Effect (EVENT())

-- | Equivalent to `newtype Listener eff args = Fn args (Eff eff Unit)
-- | though we always wrap listeners in a new function at the value level
-- | and an Eff (event :: EVENT | eff) at the type level so we can check
-- | equality of `Listeners`.
foreign import data Listener :: # ! -> * -> *

-- TODO: is this the right effect?
--       (we are just wrapping the function for equality's sake).
foreign import mkListener :: forall args e ret eff.
                             Fn args (Eff e Unit)
                          -> Eff (event :: EVENT | eff) (Listener e args)

foreign import listenerEq :: forall eff args.
                             Listener eff args
                          -> Listener eff args
                          -> Boolean

-- Instances ------------------------------------------------------------------
instance eqListener :: Eq (Listener eff args) where
  eq = listenerEq
