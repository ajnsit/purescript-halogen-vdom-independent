module Halogen.VDom.DOM
  ( VDomSpec(..)
  , buildVDom
  , buildText
  , buildElem
  , buildKeyed
  , buildWidget
  ) where

import Prelude

import Data.Array as Array
import Data.Function.Uncurried as Fn
import Data.Maybe (Maybe(..))
import Data.Nullable (toNullable)
import Data.Tuple (Tuple(..), fst)
import Effect.Uncurried as EFn
import Foreign.Object as Object
import Halogen.VDom.HostConfig (HostConfig)
import Halogen.VDom.Machine (Machine, Step, Step'(..), extract, halt, mkStep, step, unStep)
import Halogen.VDom.Machine as Machine
import Halogen.VDom.Types (ElemName(..), Namespace(..), VDom(..), runGraft)
import Halogen.VDom.Util as Util

type VDomMachine node a w = Machine (VDom a w) node

type VDomStep node a w = Step (VDom a w) node

type VDomInit node i a w = EFn.EffectFn1 i (VDomStep node a w)

type VDomBuilder node i a w = EFn.EffectFn3 (VDomSpec node a w) (VDomMachine node a w) i (VDomStep node a w)

type VDomBuilder4 node i j k l a w = EFn.EffectFn6 (VDomSpec node a w) (VDomMachine node a w) i j k l (VDomStep node a w)

-- | Widget machines recursively reference the configured spec to potentially
-- | enable recursive trees of Widgets.
newtype VDomSpec node a w = VDomSpec
  { buildWidget ∷ VDomSpec node a w → Machine w node
  , buildAttributes ∷ node → Machine a Unit
  }

-- | Starts an initial `VDom` machine by providing a `VDomSpec`.
-- |
-- | ```purescript
-- | main = do
-- |   machine1 ← buildVDom spec vdomTree1
-- |   machine2 ← Machine.step machine1 vdomTree2
-- |   machine3 ← Machine.step machine2 vdomTree3
-- |   ...
-- | ````
buildVDom ∷ ∀ a w evt node. HostConfig evt node -> VDomSpec node a w → VDomMachine node a w
buildVDom hconf spec = build
  where
  buildKeyedGo = buildKeyed hconf
  buildElemGo = buildElem hconf
  buildTextGo = buildText hconf
  build = EFn.mkEffectFn1 case _ of
    Text s → EFn.runEffectFn3 buildTextGo spec build s
    Elem ns n a ch → EFn.runEffectFn6 buildElemGo spec build ns n a ch
    Keyed ns n a ch → EFn.runEffectFn6 buildKeyedGo spec build ns n a ch
    Widget w → EFn.runEffectFn3 buildWidget spec build w
    Grafted g → EFn.runEffectFn1 build (runGraft g)

type TextState node a w =
  { build ∷ VDomMachine node a w
  , node ∷ node
  , value ∷ String
  }

buildText ∷ ∀ a w evt node. HostConfig evt node -> VDomBuilder node String a w
buildText hconf = buildTextGo
  where
  haltTextGo = haltText hconf
  patchTextGo = patchText hconf
  buildTextGo = EFn.mkEffectFn3 \(VDomSpec spec) build s → do
    node ← EFn.runEffectFn1 hconf.createTextNode s
    let state = { build, node, value: s }
    pure $ mkStep $ Step node state patchTextGo haltTextGo

patchText ∷ ∀ a w evt node. HostConfig evt node -> EFn.EffectFn2 (TextState node a w) (VDom a w) (VDomStep node a w)
patchText hconf = patchTextGo
  where
  haltTextGo = haltText hconf
  patchTextGo = EFn.mkEffectFn2 \state vdom → do
    let { build, node, value: value1 } = state
    case vdom of
      Grafted g →
        EFn.runEffectFn2 patchTextGo state (runGraft g)
      Text value2
        | value1 == value2 →
            pure $ mkStep $ Step node state patchTextGo haltTextGo
        | otherwise → do
            let nextState = { build, node, value: value2 }
            EFn.runEffectFn2 hconf.setTextContent value2 node
            pure $ mkStep $ Step node nextState patchTextGo haltTextGo
      _ → do
        EFn.runEffectFn1 haltTextGo state
        EFn.runEffectFn1 build vdom

haltText ∷ ∀ a w evt node. HostConfig evt node -> EFn.EffectFn1 (TextState node a w) Unit
haltText hconf = EFn.mkEffectFn1 \ { node } → do
  parent ← EFn.runEffectFn1 hconf.parentNode node
  EFn.runEffectFn2 hconf.removeChild node parent

type ElemState node a w =
  { build ∷ VDomMachine node a w
  , node ∷ node
  , attrs ∷ Step a Unit
  , ns ∷ Maybe Namespace
  , name ∷ ElemName
  , children ∷ Array (VDomStep node a w)
  }

buildElem ∷ ∀ a w evt node. HostConfig evt node -> VDomBuilder4 node (Maybe Namespace) ElemName a (Array (VDom a w)) a w
buildElem hconf = buildElemGo
  where
  haltElemGo = haltElem hconf
  patchElemGo = patchElem hconf
  buildElemGo = EFn.mkEffectFn6 \(VDomSpec spec) build ns1 name1 as1 ch1 → do
    node ← EFn.runEffectFn2 hconf.createElement (toNullable ns1) name1
    let
      onChild = EFn.mkEffectFn2 \ix child → do
        res ← EFn.runEffectFn1 build child
        EFn.runEffectFn3 hconf.insertChildIx ix (extract res) node
        pure res
    children ← EFn.runEffectFn2 Util.forE ch1 onChild
    attrs ← EFn.runEffectFn1 (spec.buildAttributes node) as1
    let
      state =
        { build
        , node
        , attrs
        , ns: ns1
        , name: name1
        , children
        }
    pure $ mkStep $ Step node state patchElemGo haltElemGo

patchElem ∷ ∀ a w evt node. HostConfig evt node -> EFn.EffectFn2 (ElemState node a w) (VDom a w) (VDomStep node a w)
patchElem hconf = patchElemGo
  where
  haltElemGo = haltElem hconf
  patchElemGo = EFn.mkEffectFn2 \state vdom → do
    let { build, node, attrs, ns: ns1, name: name1, children: ch1 } = state
    case vdom of
      Grafted g →
        EFn.runEffectFn2 patchElemGo state (runGraft g)
      Elem ns2 name2 as2 ch2 | Fn.runFn4 eqElemSpec ns1 name1 ns2 name2 → do
        case Array.length ch1, Array.length ch2 of
          0, 0 → do
            attrs2 ← EFn.runEffectFn2 step attrs as2
            let
              nextState =
                { build
                , node
                , attrs: attrs2
                , ns: ns2
                , name: name2
                , children: ch1
                }
            pure $ mkStep $ Step node nextState patchElemGo haltElemGo
          _, _ → do
            let
              onThese = EFn.mkEffectFn3 \ix s v → do
                res ← EFn.runEffectFn2 step s v
                EFn.runEffectFn3 hconf.insertChildIx ix (extract res) node
                pure res
              onThis = EFn.mkEffectFn2 \ix s → EFn.runEffectFn1 halt s
              onThat = EFn.mkEffectFn2 \ix v → do
                res ← EFn.runEffectFn1 build v
                EFn.runEffectFn3 hconf.insertChildIx ix (extract res) node
                pure res
            children2 ← EFn.runEffectFn5 Util.diffWithIxE ch1 ch2 onThese onThis onThat
            attrs2 ← EFn.runEffectFn2 step attrs as2
            let
              nextState =
                { build
                , node
                , attrs: attrs2
                , ns: ns2
                , name: name2
                , children: children2
                }
            pure $ mkStep $ Step node nextState patchElemGo haltElemGo
      _ → do
        EFn.runEffectFn1 haltElemGo state
        EFn.runEffectFn1 build vdom

haltElem ∷ ∀ a w evt node. HostConfig evt node -> EFn.EffectFn1 (ElemState node a w) Unit
haltElem hconf = EFn.mkEffectFn1 \ { node, attrs, children } → do
  parent ← EFn.runEffectFn1 hconf.parentNode node
  EFn.runEffectFn2 hconf.removeChild node parent
  EFn.runEffectFn2 Util.forEachE children halt
  EFn.runEffectFn1 halt attrs

type KeyedState node a w =
  { build ∷ VDomMachine node a w
  , node ∷ node
  , attrs ∷ Step a Unit
  , ns ∷ Maybe Namespace
  , name ∷ ElemName
  , children ∷ Object.Object (VDomStep node a w)
  , length ∷ Int
  }

buildKeyed ∷ ∀ a w evt node. HostConfig evt node -> VDomBuilder4 node (Maybe Namespace) ElemName a (Array (Tuple String (VDom a w))) a w
buildKeyed hconf = buildKeyedGo
  where
  patchKeyedGo = patchKeyed hconf
  haltKeyedGo = haltKeyed hconf
  buildKeyedGo = EFn.mkEffectFn6 \(VDomSpec spec) build ns1 name1 as1 ch1 → do
    node ← EFn.runEffectFn2 hconf.createElement (toNullable ns1) name1
    let
      onChild = EFn.mkEffectFn3 \k ix (Tuple _ vdom) → do
        res ← EFn.runEffectFn1 build vdom
        EFn.runEffectFn3 hconf.insertChildIx ix (extract res) node
        pure res
    children ← EFn.runEffectFn3 Util.strMapWithIxE ch1 fst onChild
    attrs ← EFn.runEffectFn1 (spec.buildAttributes node) as1
    let
      state =
        { build
        , node
        , attrs
        , ns: ns1
        , name: name1
        , children
        , length: Array.length ch1
        }
    pure $ mkStep $ Step node state patchKeyedGo haltKeyedGo

patchKeyed ∷ ∀ a w evt node. HostConfig evt node -> EFn.EffectFn2 (KeyedState node a w) (VDom a w) (VDomStep node a w)
patchKeyed hconf = patchKeyedGo
  where
  haltKeyedGo = haltKeyed hconf
  patchKeyedGo = EFn.mkEffectFn2 \state vdom → do
    let { build, node, attrs, ns: ns1, name: name1, children: ch1, length: len1 } = state
    case vdom of
      Grafted g →
        EFn.runEffectFn2 patchKeyedGo state (runGraft g)
      Keyed ns2 name2 as2 ch2 | Fn.runFn4 eqElemSpec ns1 name1 ns2 name2 →
        case len1, Array.length ch2 of
          0, 0 → do
            attrs2 ← EFn.runEffectFn2 Machine.step attrs as2
            let
              nextState =
                { build
                , node
                , attrs: attrs2
                , ns: ns2
                , name: name2
                , children: ch1
                , length: 0
                }
            pure $ mkStep $ Step node nextState patchKeyedGo haltKeyedGo
          _, len2 → do
            let
              onThese = EFn.mkEffectFn4 \_ ix' s (Tuple _ v) → do
                res ← EFn.runEffectFn2 step s v
                -- AJ: TODO: This should check if the parents are the same and then not insert the new child
                -- AJ: TODO: This is harder to detect and optimise on the backend with lesser information
                EFn.runEffectFn3 hconf.insertChildIx ix' (extract res) node
                pure res
              onThis = EFn.mkEffectFn2 \_ s → EFn.runEffectFn1 halt s
              onThat = EFn.mkEffectFn3 \_ ix (Tuple _ v) → do
                res ← EFn.runEffectFn1 build v
                EFn.runEffectFn3 hconf.insertChildIx ix (extract res) node
                pure res
            children2 ← EFn.runEffectFn6 Util.diffWithKeyAndIxE ch1 ch2 fst onThese onThis onThat
            attrs2 ← EFn.runEffectFn2 step attrs as2
            let
              nextState =
                { build
                , node
                , attrs: attrs2
                , ns: ns2
                , name: name2
                , children: children2
                , length: len2
                }
            pure $ mkStep $ Step node nextState patchKeyedGo haltKeyedGo
      _ → do
        EFn.runEffectFn1 haltKeyedGo state
        EFn.runEffectFn1 build vdom

haltKeyed ∷ ∀ a w evt node. HostConfig evt node -> EFn.EffectFn1 (KeyedState node a w) Unit
haltKeyed hconf = EFn.mkEffectFn1 \ { node, attrs, children } → do
  parent ← EFn.runEffectFn1 hconf.parentNode node
  EFn.runEffectFn2 hconf.removeChild node parent
  EFn.runEffectFn2 Util.forInE children (EFn.mkEffectFn2 \_ s → EFn.runEffectFn1 halt s)
  EFn.runEffectFn1 halt attrs

type WidgetState node a w =
  { build ∷ VDomMachine node a w
  , widget ∷ Step w node
  }

buildWidget ∷ ∀ a w node. VDomBuilder node w a w
buildWidget = EFn.mkEffectFn3 \(VDomSpec spec) build w → do
  res ← EFn.runEffectFn1 (spec.buildWidget (VDomSpec spec)) w
  let
    res' = res # unStep \(Step n s k1 k2) →
      mkStep $ Step n { build, widget: res } patchWidget haltWidget
  pure res'

patchWidget ∷ ∀ a w node. EFn.EffectFn2 (WidgetState node a w) (VDom a w) (VDomStep node a w)
patchWidget = EFn.mkEffectFn2 \state vdom → do
  let { build, widget } = state
  case vdom of
    Grafted g →
      EFn.runEffectFn2 patchWidget state (runGraft g)
    Widget w → do
      res ← EFn.runEffectFn2 step widget w
      let
        res' = res # unStep \(Step n s k1 k2) →
          mkStep $ Step n { build, widget: res } patchWidget haltWidget
      pure res'
    _ → do
      EFn.runEffectFn1 haltWidget state
      EFn.runEffectFn1 build vdom

haltWidget ∷ forall a w node. EFn.EffectFn1 (WidgetState node a w) Unit
haltWidget = EFn.mkEffectFn1 \ { widget } → do
  EFn.runEffectFn1 halt widget

eqElemSpec ∷ Fn.Fn4 (Maybe Namespace) ElemName (Maybe Namespace) ElemName Boolean
eqElemSpec = Fn.mkFn4 \ns1 (ElemName name1) ns2 (ElemName name2) →
  if name1 == name2
    then case ns1, ns2 of
      Just (Namespace ns1'), Just (Namespace ns2') | ns1' == ns2' → true
      Nothing, Nothing → true
      _, _ → false
    else false

-- A Simple decorated-vdom machine that just delegates everything to the underlying vdom
buildDecoration
  ∷ ∀ decor a w evt node
  . HostConfig evt node
  → (decor (VDom a w) -> VDom a w)
  → VDomSpec node a w
  → Machine (decor (VDom a w)) node
buildDecoration hconf runDecoration = renderDecoration
  where
  renderDecoration ∷ VDomSpec node a w → Machine (decor (VDom a w)) node
  renderDecoration spec = EFn.mkEffectFn1 \t → do
    vdom ← EFn.runEffectFn1 (buildVDom hconf spec) (runDecoration t)
    pure $ mkStep $ Step (extract vdom) vdom patchDecoration halt

  patchDecoration ∷ EFn.EffectFn2 (Step (VDom a w) node) (decor (VDom a w)) (Step (decor (VDom a w)) node)
  patchDecoration = EFn.mkEffectFn2 \vdomPrev t → do
    vdom ← EFn.runEffectFn2 step vdomPrev (runDecoration t)
    pure $ mkStep $ Step (extract vdom) vdom patchDecoration halt
