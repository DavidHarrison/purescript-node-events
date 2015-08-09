module Node.Events.Types (EVENT(), Event(..), Listener) where

foreign import data EVENT :: !

newtype Event args = Event String

-- | Equivalent to `newtype Listener eff args = Fn args (Eff eff Unit)
-- | though we always wrap listeners in a new function at the value level
-- | and an Eff (event :: EVENT | eff) at the type level so we can check
-- | equality of `Listeners`.
foreign import data Listener :: # ! -> * -> *
