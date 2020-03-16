module Mado
  ( Slot
  , Query (..)
  , Output (..)
  , ModalState (..)
  , component
  ) where

import Prelude

import Data.Symbol ( SProxy(..) )
import Data.Maybe ( Maybe(..) )
import Data.Foldable ( foldl )
-- Halogen
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Core as HC
import Halogen.HTML.Events as HE

-- | Slot
type Slot q o = H.Slot ( Query q ) ( Output o )

type ChildSlot q o =
  ( inner :: H.Slot q o Unit
  )

-- | The modal's internal state.
type State i =
  { modalState :: ModalState
  , input :: i
  }

-- | Query
data Query ( q :: Type -> Type ) a
  = ChangeModalState ModalState a

-- | Output of the modal
data Output o
  = Opened
  | Closed

-- | Possible Action of the modal
data Action o
  = HandleClose

-- | Possible states of the modal
data ModalState
  = Open
  | Close

-- | This component is a higher order component that will wrap other components
component
  :: forall q i o m
  . H.Component HH.HTML q i o m
  -> H.Component HH.HTML ( Query q ) i ( Output o ) m
component innerComponent =
  H.mkComponent
  { initialState: { input: _,  modalState : Close }
  , render: render innerComponent
  , eval: H.mkEval $ H.defaultEval
    { handleAction = handleAction
    , handleQuery = handleQuery
    }
  }

render
  :: forall q i o m
  . H.Component HH.HTML q i o m
  -> State i
  -> H.ComponentHTML ( Action o ) ( ChildSlot q o ) m
render innerComponent state = case state.modalState of
  Open ->
    HH.div
    [ css "modal"
    , styleProps
      [ { styleName: "display"
        , styleValue: toStyleValue state.modalState
        }
      ]
    ]
    [ HH.div
      [ css "modal-content" ]
      [ HH.span
        [ css "close"
        , HE.onClick ( const $ Just HandleClose )
        ]
        []
        , HH.slot ( SProxy :: _ "inner" ) unit innerComponent state.input ( const Nothing )
      ]
    ]
  Close -> HH.text mempty

handleAction
  :: forall q i o m
  . Action o
  -> H.HalogenM ( State i ) ( Action o ) ( ChildSlot q o ) ( Output o ) m Unit
handleAction = case _ of
  HandleClose -> do
    H.modify_ _ { modalState = Close }
    H.raise Closed

handleQuery
  :: forall f a i o m
  . Query f a
  -> H.HalogenM ( State i ) ( Action o ) ( ChildSlot f o ) ( Output o ) m ( Maybe a )
handleQuery = case _ of
  ChangeModalState state a -> do
    H.modify_ _ { modalState = state }
    pure ( Just a )

css :: forall r i. String -> HH.IProp ( class :: String | r ) i
css = HP.class_ <<< HH.ClassName

class StyleValue v where
  toStyleValue :: v -> String

instance modalStateStyleValue :: StyleValue ModalState where
  toStyleValue Open = "block"
  toStyleValue Close = "none"

type Style =
  { styleName :: String
  , styleValue :: String
  }

styleProps :: forall r i. Array Style -> HP.IProp ( style :: String | r ) i
styleProps = HH.prop ( HC.PropName "style" )
  <<< foldl (\b a -> a.styleName <> ": " <> a.styleValue <> "; " <> b ) ""
