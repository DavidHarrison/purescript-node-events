module Test.Listener where

import Control.Monad.Eff          (runPure)
import Control.Monad.Eff.Unsafe   (unsafeInterleaveEff)
import Data.Function.Multivariate (Fn())
import Test.Spec                  (describe)
import Test.Spec.QuickCheck       (quickCheck)

type A = Int
type B = Boolean
type C = String

listenerSpec :: 
listenerSpec = describe "Listener" do
  it "is equal to itself" do
    quickCheck refl
  it "is not equal to another listener with the same function" do
    quickCheck noRefl

refl :: Fn (Cons A (Cons B Nil)) C -> Boolean
refl f = runPure $ unsafeInterleaveEff do
  let f' = mkEffFn f
  l <- mkListener f'
  return (l == l)

noRefl :: Fn (Cons A (Cons B Nil)) C -> Boolean
noRefl f = runPure $ unsafeInterleaveEff do
  let f' = mkEffFn f
  l  <- mkListener f'
  l' <- mkListener f'
  return (l /= l')
