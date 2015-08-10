module Node.Events.Aff where -- (listen, listenOnce, withListener) where

import Prelude

import Control.Monad.Aff          (Aff(), makeAff)
import Control.Monad.Eff          (Eff())
import Data.Function.Multivariate (Fn())
import Data.Iso                   (Iso(), forwards)

-- import Node.Events          (Emits, addListener, once)
import Node.Events.Listener (Listener(), mkListener)
import Node.Events.Effect   (EVENT())

{-
listen :: forall emitter event args eff.
          (Emits emitter event args)
       => Iso (args -> Eff eff Unit) (Fn args (Eff eff Unit))
       -> event
       -> emitter
       -> Aff (e :: EVENT | eff) args
listen iso event emitter = withListener addListener'
  where addListener' listener = addListener event listener emitter

listenOnce :: forall emitter event args eff.
              (Emits emitter event args)
           => Iso (args -> Eff eff Unit) (Fn args (Eff eff Unit))
           -> event
           -> emitter
           -> Aff (e :: EVENT | eff) args
listenOnce iso event emitter = withListener iso once'
  where once' listener = once event listener emitter

withListener :: forall eff args.
                Iso (args -> Eff eff Unit) (Fn args (Eff eff Unit))
             -> (Listener eff args -> Eff (event :: EVENT | eff) Unit)
             -> Aff (event :: EVENT | eff) args
withListener iso f = makeAff fEff
  where fEff _ success = mkListener (forwards iso success) >>= f
-}
