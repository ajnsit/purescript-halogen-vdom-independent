module Halogen.VDom.Machine
  ( Machine
  , Step'(..)
  , Step
  , mkStep
  , unStep
  , extract
  , step
  , halt
  ) where

import Prelude

import Data.Profunctor (class Profunctor, dimap)
import Effect.Uncurried (EffectFn1, EffectFn2, mkEffectFn1, mkEffectFn2, runEffectFn1, runEffectFn2)
import Unsafe.Coerce (unsafeCoerce)

type Machine a b = EffectFn1 a (Step a b)

data Step' a b s = Step b s (EffectFn2 s a (Step a b)) (EffectFn1 s Unit)

foreign import data Step ∷ Type → Type → Type

mkStep ∷ ∀ a b s. Step' a b s → Step a b
mkStep = unsafeCoerce

unStep :: ∀ a b r. (∀ s. Step' a b s → r) → Step a b → r
unStep = unsafeCoerce

instance profunctorStep :: Profunctor Step where
  dimap l r = unStep \(Step b s m h) ->
    let m' = mkEffectFn2 \s0 a -> dimap l r <$> runEffectFn2 m s0 (l a)
    in mkStep $ Step (r b) s m' h

-- | Returns the output value of a `Step`.
extract ∷ ∀ a b. Step a b → b
extract = unStep \(Step x _ _ _) → x

-- | Runs the next step.
step ∷ ∀ a b. EffectFn2 (Step a b) a (Step a b)
step = coerce $ mkEffectFn2 \(Step _ s k _) a → runEffectFn2 k s a
  where
  coerce ∷ ∀ s. EffectFn2 (Step' a b s) a (Step a b) → EffectFn2 (Step a b) a (Step a b)
  coerce = unsafeCoerce

-- | Runs the finalizer associated with a `Step`
halt ∷ ∀ a b. EffectFn1 (Step a b) Unit
halt = coerce $ mkEffectFn1 \(Step _ s _ k) → runEffectFn1 k s
  where
  coerce ∷ ∀ s. EffectFn1 (Step' a b s) Unit → EffectFn1 (Step a b) Unit
  coerce = unsafeCoerce
