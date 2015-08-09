-- TODO: are we using `Int`s correctly
module Node.Events where

import Prelude

import Data.Function.Multivariate(Fn(), Cons(), Nil(), Isomorphic())
import Data.Nullable (toNullable)
import Unsafe.Coerce (unsafeCoerce)

import Node.Events.Types (EVENT(), Event(..), Listener())
import Node.Events.Unsafe (EE(..))

-- TODO: is this the right effect?
--       (we are just wrapping the function for equality's sake).
--       Is there an allocation effect or similar?
foreign import mkListener :: forall args e ret eff.
                             Fn args (Eff e Unit)
                          -> Eff (event :: EVENT | eff) (Listener e args)

toListener :: forall f e eff.
              (Isomorphic f (Fn args (Eff e Unit)))
           => f
           -> Eff (event :: EVENT | eff) (Listener e args)
toListener = mkListener <<< to

-- | `toObject` will generally be implemented as `unsafeCoerce`.
class EventEmitter emitter where
  toObject :: emitter -> EE

instance EventEmitter EE where
  toObject = id

-- | NOTE: the combination of `event` and `args` should be unique.
class (EventEmitter emitter) <= Emits emitter event args where
  name :: event -> Event args

-- API ------------------------------------------------------------------------
-- Instance Methods -----------------------------------------------------------
addListener :: forall emitter event args eff.
               (Emits emitter event args)
            => event
            -> Listener eff args
            -> emitter
            -> Eff (e :: EVENT | eff) emitter
addListener event listener emitter = addListener' event listener
  where addListener' = mkEff' (toObject emitter).addListener

once :: forall emitter event args eff.
        (Emits emitter event args)
     => event
     -> Listener eff args
     -> emitter
     -> Eff (e :: EVENT | eff) emitter
once event listener emitter = once' event lister
  where addListener' = mkEff' (toObject emitter).once

removeListener :: forall emitter event args eff.
                  (Emits emitter event args)
               => Listener eff args
               -> emitter
               -> Eff (e :: EVENT | eff) emitter
removeListener listener emitter = removeListener' listener
  where removeListener' = mkEff' (toObject emitter).removeListener

removeListenersFor :: forall emitter event args eff.
                      (Emits emitter event args)
                   => event
                   -> emitter
                   -> Eff (e :: EVENT | eff) emitter
removeListenersFor event emitter = removeAllListeners' (toNullable (Just event))
  where removeAllListeners' = mkEff' (toObject emitter).removeAllListeners

removeAllListeners :: forall emitter eff.
                      (EventEmitter emitter)
                   => emitter
                   -> Eff (e :: EVENT | eff) emitter
removeAllListeners emitter = removeAllListeners' (toNullable Nothing)
  where removeAllListeners' = mkEff' (toObject emitter).removeAllListeners

setMaxListeners :: forall emitter eff.
                   (EventEmitter emitter)
                => Int
                -> emitter
                -> Eff (e :: EVENT | eff) emitter
setMaxLiseners n emitter = setMaxListeners' n
  where setMaxListeners' = mkEff' (toObject emitter).setMaxListeners

listeners :: forall emitter event args eff.
             (Emits emitter event args)
          => event
          -> emitter
          -> Eff (e :: EVENT | eff) (Array (Listener eff args))
listeners event emitter = listeners' event
  where listeners' = mkEff (toObject emitter).listeners

-- TODO, move emitter to last argument?
emit :: forall emitter event args eff.
        ( Emits emitter event args
        , Isomorphic (Fn args (Eff (e :: EVENT | eff) emitter)) f
        )
     => event
     -> emitter
     -> f
emit event emitter = emit' event
  where emit' = mkEff (toObject emitter).emit

-- Class Methods --------------------------------------------------------------
foreign import newEventEmitter :: forall eff. Eff (e :: EVENT | eff) EE

foreign import getDefaultMaxListeners :: forall eff. Eff (e :: EVENT | eff) Int

foreign import setDefaultMaxListeners :: forall eff.
                                         Int
                                      -> Eff (e :: EVENT | eff) Unit

foreign import listenerCount :: forall emitter event args eff.
                                (Emits emitter event args)
                             => event
                             -> emitter
                             -> Int

-- Universal Listeners --------------------------------------------------------
data NewListener
data RemoveListener

type EventListener = forall args eff.
                     Cons (Event args) (Cons (Listener eff args) Nil)

instance (EventEmitter emitter) => Emits emitter NewListener EventListener where
  name _ = "newListener"

instance (EventEmitter emitter) => Emits emitter RemoveListener EventListener where
  name _ = "removeListener"

-- Listener Instances ---------------------------------------------------------

foreign import listenerEq :: forall eff args.
                             Listener eff args
                          -> Listener eff args
                          -> Boolean

instance eqListener :: Eq (Listener eff args) where
  eq = listenerEq

-- Helper Functions -----------------------------------------------------------
mkEff :: forall a eff. (Unit -> a) -> Eff eff a
mkEff = unsafeCoerce

mkEff' :: forall args ret eff.
          (Isomorphic (Fn args (Eff eff ret)) f)
       => Fn args ret
       -> f
mkEff' = to <<< map (mkEff <<< const)
