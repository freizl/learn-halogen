module ParentChildRelationships.ParentlikeComponents.SingleChild.MessageOnly where

import Prelude

-- Imports for lesson
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH

-- Imports for scaffolding
import Data.Const (Const)
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Halogen (get, put)
import Halogen as H
import Halogen.Aff (awaitBody)
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)

type ChildSlots = (child :: H.Slot (Const Void) ChildMessage Unit)
type ParentState = String
type ParentQuery = Const Void
type ParentInput = Unit
type ParentMessage = Void

data ParentAction = UpdateState ParentState


main :: Effect Unit
main = do
  launchAff_ do
    body <- awaitBody
    runUI parentComponent unit body


_child :: SProxy "child"
_child = SProxy

parentComponent :: H.Component HH.HTML ParentQuery ParentInput ParentMessage Aff
parentComponent =
    H.mkComponent
      { initialState: const "empty"
      , render: renderParentWithMessageOnlyChild
      , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
      }
  where
    handleAction :: ParentAction
                 -> H.HalogenM String ParentAction ChildSlots ParentMessage Aff Unit
    handleAction = case _ of
      UpdateState str -> do
        put str

    renderParentWithMessageOnlyChild :: ParentState -> H.ComponentHTML ParentAction ChildSlots Aff
    renderParentWithMessageOnlyChild parentState =
      HH.div_
        [ HH.div_ [ HH.text "This is the parent component " ]
        , HH.div_ [ HH.text $ "Received from child: " <> parentState ]
        , HH.slot _child unit childComponent unit (\(ChildMessage childMessage) -> Just $ UpdateState childMessage)

        ]

-- | A child component that renders dynamic html. It has state and
-- | can raise messages, but it does not respond to input or to queries.
data ChildMessage = ChildMessage String
data ChildAction = NotifyParent

childComponent :: H.Component HH.HTML (Const Void) Unit ChildMessage Aff
childComponent =
    H.mkComponent
      { initialState: initialChildState
      , render: render
      , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
      }
  where
    initialChildState :: Unit -> Int
    initialChildState _ = 0

    render :: Int -> H.ComponentHTML ChildAction () Aff
    render _ =
      HH.div_
        [ HH.button
          [ HE.onClick \_ -> Just NotifyParent ]
          [ HH.text "Click me to raise a message to my parent. " ]
        ]

    handleAction :: ChildAction
                 -> H.HalogenM Int ChildAction () ChildMessage Aff Unit
    handleAction = case _ of
      NotifyParent -> do
        state <- get
        H.raise $ ChildMessage $ show state
        put (state + 1)
