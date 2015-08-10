module Node.Events.Unsafe.Object where

import Data.Exists.Row            (RowExists(), runRowExists, mkRowExists)
import Data.Function.Multivariate (Fn(), Cons(), Nil())
import Data.Nullable              (Nullable())

import Node.Events.Listener     (Listener())
import Node.Events.Unsafe.Event (Event())

newtype Emitter = Emitter (RowExists EmitterR)

newtype EmitterR r = EmitterR
  -- addListener and `on` are synonymous
  { addListener        :: forall args eff.
                          Fn (Cons (Event args) (Cons (Listener eff args) Nil))
                             Emitter
  , once               :: forall args eff.
                          Fn (Cons (Event args) (Cons (Listener eff args) Nil))
                             Emitter
  , removeListener     :: forall args eff.
                          Fn (Cons (Listener eff args) Nil)
                             Emitter
  , removeAllListeners :: forall args eff.
                          Fn (Cons (Nullable (Event args)) Nil)
                             Emitter
  , setMaxListeners    :: forall eff.
                          Fn (Cons Int Nil)
                             Emitter
  , listeners          :: forall args eff.
                          Fn (Cons (Event args) Nil)
                             (Array (Listener eff args))
  , emit               :: forall args eff.
                          Fn (Cons (Event args) args)
                             Boolean
  | r }

mkEmitter :: forall r. EmitterR r -> Emitter
mkEmitter e = Emitter (mkRowExists e)

runEmitter :: forall f a. (forall r. EmitterR r -> a) -> Emitter -> a
runEmitter f (Emitter e) = runRowExists f e

addListener :: forall args eff.
               Emitter
            -> Fn (Cons (Event args) (Cons (Listener eff args) Nil)) Emitter
addListener = runEmitter get
  where get :: forall r.
               EmitterR r
            -> Fn (Cons (Event args) (Cons (Listener eff args) Nil)) Emitter
        get (EmitterR o) = o.addListener

{-
once = runEmitter get
  where get (EmitterR o) = o.once

removeListener = runEmitter get
  where get (EmitterR o) = o.removeListener

removeAllListeners = runEmitter get
  where get (EmitterR o) = o.removeAllListeners

setMaxListeners = runEmitter get
  where get (EmitterR o) = o.setMaxListeners

listeners = runEmitter get
  where get (EmitterR o) = o.listeners

emit = runEmitter get
  where get (EmitterR o) = o.emit
-}
