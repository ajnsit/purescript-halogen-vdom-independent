module Halogen.VDom.Thunk
  ( Thunk
  , buildThunk
  , runThunk
  , thunked
  , thunk1
  , thunk2
  , thunk3
  ) where

import Prelude

import Data.Function.Uncurried as Fn
import Effect.Uncurried as EFn
import Halogen.VDom.Machine as M
import Halogen.VDom.Util as Util
import Unsafe.Coerce (unsafeCoerce)

foreign import data ThunkArg ∷ Type

foreign import data ThunkId ∷ Type

data Thunk a = Thunk ThunkId (Fn.Fn2 ThunkArg ThunkArg Boolean) (ThunkArg → a) ThunkArg

unsafeThunkId ∷ ∀ a. a → ThunkId
unsafeThunkId = unsafeCoerce

instance functorThunk ∷ Functor Thunk where
  map f (Thunk a b c d) = Thunk a b (f <<< c) d

thunk ∷ ∀ a b. Fn.Fn4 ThunkId (Fn.Fn2 a a Boolean) (a → b) a (Thunk b)
thunk = Fn.mkFn4 \tid eqFn f a →
  Thunk tid
    (unsafeCoerce eqFn ∷ Fn.Fn2 ThunkArg ThunkArg Boolean)
    (unsafeCoerce f ∷ ThunkArg → b)
    (unsafeCoerce a ∷ ThunkArg)

thunked ∷ ∀ a b. (a → a → Boolean) → (a → b) → a → Thunk b
thunked eqFn f =
  let
    tid = unsafeThunkId { f }
    eqFn' = Fn.mkFn2 eqFn
  in
    \a → Fn.runFn4 thunk tid eqFn' f a

thunk1 ∷ ∀ a b. Fn.Fn2 (a → b) a (Thunk b)
thunk1 = Fn.mkFn2 \f a → Fn.runFn4 thunk (unsafeThunkId f) Util.refEq f a

thunk2 ∷ ∀ a b c. Fn.Fn3 (a → b → c) a b (Thunk c)
thunk2 =
  let
    eqFn = Fn.mkFn2 \a b →
      Fn.runFn2 Util.refEq a._1 b._1 &&
      Fn.runFn2 Util.refEq a._2 b._2
  in
    Fn.mkFn3 \f a b →
      Fn.runFn4 thunk (unsafeThunkId f) eqFn (\ { _1, _2 } → f _1 _2) { _1: a, _2: b }

thunk3 ∷ ∀ a b c d. Fn.Fn4 (a → b → c → d) a b c (Thunk d)
thunk3 =
  let
    eqFn = Fn.mkFn2 \a b →
      Fn.runFn2 Util.refEq a._1 b._1 &&
      Fn.runFn2 Util.refEq a._2 b._2 &&
      Fn.runFn2 Util.refEq a._3 b._3
  in
    Fn.mkFn4 \f a b c →
      Fn.runFn4 thunk (unsafeThunkId f) eqFn (\ { _1, _2, _3 } → f _1 _2 _3) { _1: a, _2: b, _3: c }

runThunk ∷ ∀ a. Thunk a → a
runThunk (Thunk _ _ render arg) = render arg

unsafeEqThunk ∷ ∀ a. Fn.Fn2 (Thunk a) (Thunk a) Boolean
unsafeEqThunk = Fn.mkFn2 \(Thunk a1 b1 _ d1) (Thunk a2 b2 _ d2) →
  Fn.runFn2 Util.refEq a1 a2 &&
  Fn.runFn2 Util.refEq b1 b2 &&
  Fn.runFn2 b1 d1 d2

type ThunkState node x =
  { thunk ∷ Thunk x
  , step ∷ M.Step x node
  }

buildThunk
  ∷ ∀ x node
  . (EFn.EffectFn1 x (M.Machine x node))
  → M.Machine (Thunk x) node
buildThunk mkMachine = renderThunk
  where
  renderThunk ∷ M.Machine (Thunk x) node
  renderThunk = EFn.mkEffectFn1 \t → do
    let x = runThunk t
    machine <- EFn.runEffectFn1 mkMachine x
    step ← EFn.runEffectFn1 machine x
    pure $ M.mkStep $ M.Step (M.extract step) { thunk: t, step } patchThunk haltThunk

  patchThunk ∷ EFn.EffectFn2 (ThunkState node x) (Thunk x) (M.Step (Thunk x) node)
  patchThunk = EFn.mkEffectFn2 \state t2 → do
    let { step: prev, thunk: t1 } = state
    if Fn.runFn2 unsafeEqThunk t1 t2
      then pure $ M.mkStep $ M.Step (M.extract prev) state patchThunk haltThunk
      else do
        step ← EFn.runEffectFn2 M.step prev (runThunk t2)
        pure $ M.mkStep $ M.Step (M.extract step) { step, thunk: t2 } patchThunk haltThunk

  haltThunk ∷ EFn.EffectFn1 (ThunkState node x) Unit
  haltThunk = EFn.mkEffectFn1 \state → do
    EFn.runEffectFn1 M.halt state.step
