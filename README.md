# Mado

Mado is a halogen modal component

## Installation
Add `mado` to your `package.dhall`
``` sh
let additions =
  { mado =
      { dependencies =
        [ "halogen"
        ]
      , repo = "https://taezos.org/taezos/purescript-halogen-mado.git"
      , version = "v1.0"
      }
  }
```
Then add it as a dependency in your `spago.dhall`

## Usage
This module exports the following
- Slot
- Query
- Output
- ModalState
- component

Here's an example on how it's used.
```haskell
module Component.Container where

import Prelude
import Data.Maybe ( Maybe (..) )
import Data.Symbol ( SProxy(..) )
-- Internal
import Component.Title as Title
-- Halogen
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
-- Halogen Components
import Mado as Mado

type ChildSlot =
  ( modal :: forall query. Mado.Slot query Void Unit
  )

data Action = OpenModal

_modal :: SProxy "modal"
_modal = SProxy

component :: forall q i o m. H.Component HH.HTML q i o m
component =
  H.mkComponent
  { initialState: identity
  , render
  , eval: H.mkEval H.defaultEval
    { handleAction = handleAction }
  }

render :: forall state m. state -> H.ComponentHTML Action ChildSlot m
render state =
  HH.div_
  [ HH.button
    [ HE.onClick ( const $ Just OpenModal )
    ]
    [ HH.text "Open"]
  , HH.slot _modal unit modalComponent unit ( const Nothing )
  ]

handleAction :: forall state o m. Action -> H.HalogenM state Action ChildSlot o m Unit
handleAction = case _ of
  OpenModal ->
    void $ H.query _modal unit $ H.tell ( Mado.ChangeModalState Mado.Open )

modalComponent
  :: forall q o m
  . H.Component HH.HTML ( Mado.Query q ) Unit ( Mado.Output o ) m
modalComponent = Mado.component Title.component

```
and this it the `Title` component
``` haskell
component :: forall q i o m. H.Component HH.HTML q i o m
component =
  H.mkComponent
  { initialState: identity
  , render
  , eval: H.mkEval H.defaultEval
  }

render :: forall state action m. state -> H.ComponentHTML action () m
render _ =
  HH.h1_ [ HH.text "Hello, World!" ]
```
