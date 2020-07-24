module Halogen.VDom.HostConfig where

import Data.Nullable (Nullable)
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Uncurried as EFn
import Halogen.VDom.Types (ElemName, Namespace)

type EventType = String

-- An abstract type
data EventListener evt

type HostConfig evt node =
  { createTextNode ∷ EFn.EffectFn1 String node
  , setTextContent ∷ EFn.EffectFn2 String node Unit
  , createElement ∷ EFn.EffectFn2 (Nullable Namespace) ElemName node
  , insertChildIx ∷ EFn.EffectFn3 Int node node Unit
  , removeChild ∷ EFn.EffectFn2 node node Unit
  , parentNode ∷ EFn.EffectFn1 node node
  , setAttribute ∷ EFn.EffectFn4 (Nullable Namespace) String String node Unit
  , removeAttribute ∷ EFn.EffectFn3 (Nullable Namespace) String node Unit
  , hasAttribute ∷ EFn.EffectFn3 (Nullable Namespace) String node Boolean
  -- This is needed because eventlisteners are compared by ref equality
  -- i.e. the *exact* same event listener must be passed to both add/removeEventListener
  , makeEventListener ∷ EFn.EffectFn1 (evt -> Effect Unit) (EventListener evt)
  , addEventListener ∷ EFn.EffectFn3 EventType (EventListener evt) node Unit
  , removeEventListener ∷ EFn.EffectFn3 EventType (EventListener evt) node Unit
  }

-- class HostConfig node where
--   impl :: HostConfigRec node
