-- TODO: are we using `Int`s safely in FFI?
-- TODO: Re-exports
module Node.Events
  ( EventEmitter, emitter
  , Emits, event
  , NewListener(..)
  , RemoveListener(..)
  , addListener
  {-
  , once
  , removeListener
  , removeListenersFor
  , removeAllListeners
  , setMaxListeners
  , listeners
  , emit
  , newEventEmitter
  , getDefaultMaxListeners
  , setDefaultMaxListeners
  , listenerCount
  -}
  ) where

import Prelude

import Control.Monad.Eff          (Eff())
import Data.Function.Multivariate (Fn(), Cons(), Nil(), fromMulti)
import Data.Iso                   (Iso())
import Data.Maybe                 (Maybe(..))
import Data.Nullable              (toNullable)
import Data.Tuple                 (Tuple(..))
import Unsafe.Coerce              (unsafeCoerce)

import Node.Events.Listener      (Listener())
import Node.Events.Effect        (EVENT())
import Node.Events.Unsafe.Event  (Event(..))
import qualified Node.Events.Unsafe.Object as Obj

-- Classes --------------------------------------------------------------------
-- | `toObject` will generally be implemented as `unsafeCoerce`.
class EventEmitter emitter where
  emitter :: emitter -> Obj.Emitter

instance eventEmitterEmitter :: EventEmitter Obj.Emitter where
  emitter = id

-- | NOTE: the combination of `event` and `args` should be unique.
class (EventEmitter emitter) <= Emits emitter event args where
  event :: event -> Event args

-- API ------------------------------------------------------------------------
-- Instance Methods -----------------------------------------------------------
addListener :: forall emitter event args eff.
               (Emits emitter event args)
            => event
            -> Listener eff args
            -> emitter
            -> Eff (e :: EVENT | eff) emitter
addListener ev l em = addListener' (Tuple (event ev) (Tuple l unit))
  where addListener' :: (Tuple (Event args) (Tuple (Listener eff args) Unit)) -> Eff (e :: EVENT | eff) emitter
        addListener' = fromMulti $ mkEffFn $ Obj.addListener $ emitter em

{-
once :: forall emitter event args eff.
        (Emits emitter event args)
     => event
     -> Listener eff args
     -> emitter
     -> Eff (e :: EVENT | eff) emitter
once event listener emitter = fromMulti (once' event listener)
  where
    once' :: event -> Listener eff args -> Fn args (Eff (e :: EVENT | eff) emitter)
    once' = mkEffFn obj.once
    obj = runEmitter (emitter 

removeListener :: forall emitter event args eff.
                  (Emits emitter event args)
               => Listener eff args
               -> emitter
               -> Eff (e :: EVENT | eff) emitter
removeListener listener emitter = fromMulti (removeListener' listener)
  where
    removeListener' :: Listener eff args -> Fn args (Eff (e :: EVENT | eff) emitter)
    removeListener' = mkEffFn obj.removeListener
    obj :: forall r. Emitter r
    obj = toObject emitter

removeListenersFor :: forall emitter event args eff.
                      (Emits emitter event args)
                   => event
                   -> emitter
                   -> Eff (e :: EVENT | eff) emitter
removeListenersFor event emitter = removeAllListeners' (toNullable (Just event))
  where removeAllListeners' = mkEffFn (toObject emitter).removeAllListeners

removeAllListeners :: forall emitter eff.
                      (EventEmitter emitter)
                   => emitter
                   -> Eff (e :: EVENT | eff) emitter
removeAllListeners emitter = removeAllListeners' (toNullable Nothing)
  where removeAllListeners' = mkEffFn (toObject emitter).removeAllListeners

setMaxListeners :: forall emitter eff.
                   (EventEmitter emitter)
                => Int
                -> emitter
                -> Eff (e :: EVENT | eff) emitter
setMaxListeners n emitter = setMaxListeners' n
  where setMaxListeners' = mkEffFn (toObject emitter).setMaxListeners

listeners :: forall emitter event args eff.
             (Emits emitter event args)
          => event
          -> emitter
          -> Eff (e :: EVENT | eff) (Array (Listener eff args))
listeners event emitter = listeners' event
  where listeners' = mkEff (toObject emitter).listeners

-- TODO, move emitter to last argument?
emit :: forall emitter event args eff.
        (Emits emitter event args)
     => Iso f (Fn args (Eff (e :: EVENT | eff) emitter))
     -> event
     -> args
     -> emitter
emit event args emitter = emit' event
  where emit' = mkEff (toObject emitter).emit

-- Class Methods --------------------------------------------------------------
foreign import newEventEmitter :: forall eff r. Eff (e :: EVENT | eff) Emitter

foreign import getDefaultMaxListeners :: forall eff. Eff (e :: EVENT | eff) Int

foreign import setDefaultMaxListeners :: forall eff.
                                         Int
                                      -> Eff (e :: EVENT | eff) Unit

foreign import listenerCount :: forall emitter event args eff.
                                (Emits emitter event args)
                             => event
                             -> emitter
                             -> Int
-}

-- Universal Listeners --------------------------------------------------------
data NewListener = NewListener
data RemoveListener = RemoveListener


instance emitsNewListener :: (EventEmitter emitter)
                          => Emits emitter
                                   NewListener
                                   (Tuple (Event args)
                                          (Tuple (Listener eff args) Unit)) where
  event _ = Event "newListener"

instance emitsRemoveListener :: (EventEmitter emitter)
                             => Emits emitter
                                      RemoveListener
                                      (Tuple (Event args)
                                             (Tuple (Listener eff args) Unit)) where
  event _ = Event "removeListener"

-- Helper Functions -----------------------------------------------------------
mkEff :: forall a eff. (Unit -> a) -> Eff eff a
mkEff = unsafeCoerce

mkEffFn :: forall args ret eff. Fn args ret -> Fn args (Eff eff ret)
mkEffFn = map (mkEff <<< const)
