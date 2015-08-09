module Node.Events.Unsafe (EE(..)) where

import Node.Events.Types (Event(..), Listener)

newtype EE = EE forall r.
  -- addListener and `on` are synonymous
  { addListener        :: forall eff args.
                          Fn (Cons (Event args) (Cons (Listener eff args) Nil))
                             EE
  , once               :: forall args eff.
                          Fn (Cons (Event args) (Cons (Listener eff args) Nil))
                             EE
  , removeListener     :: forall args eff.
                          Fn (Cons (Listener eff args) Nil)
                             EE
  , removeAllListeners :: forall args eff.
                          Fn (Cons (Nullable (Event args)) Nil)
                             EE
  , setMaxListeners    :: forall eff.
                          Fn (Cons Int Nil)
                             EE
  , listeners          :: forall args eff.
                          Fn (Cons (Event args) Nil)
                             (Array (Listener eff args))
  , emit               :: forall args eff.
                          Fn (Cons (Event args) args)
                             Boolean
  | r }
