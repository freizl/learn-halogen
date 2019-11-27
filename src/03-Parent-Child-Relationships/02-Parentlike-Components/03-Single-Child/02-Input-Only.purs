module ParentChildRelationships.ParentlikeComponents.SingleChild.InputOnly where

import Prelude

-- Imports for lesson
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

-- Imports for scaffolding
import CSS (backgroundColor, fontSize, orange, padding, px)
import Data.Const (Const)
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Random (randomInt)
import Halogen (liftEffect, put)
import Halogen as H
import Halogen.Aff (awaitBody)
import Halogen.HTML.CSS as CSS
import Halogen.VDom.Driver (runUI)


-- | The parent's only action is `RandomState`
data ParentAction = RandomState

data ParentState = ParentState Int

type ChildSlots =
    (child :: H.Slot
                (Const Unit) -- no query type
                Void         -- no message type
                Unit         -- single child, so use Unit for slot index
    )

_child :: SProxy "child"
_child = SProxy

main :: Effect Unit
main  = do
  launchAff_ do
    body <- awaitBody
    initialInt <- liftEffect $ randomInt 1 200
    runUI parentComponent initialInt body

-- | Wraps Halogen types cleanly, so that one gets very clear compiler errors
parentComponent :: H.Component HH.HTML (Const Unit) Int Void Aff
parentComponent =
    H.mkComponent
      { initialState: \int -> ParentState int
      , render: \state -> renderParentWithInputOnlyChild simpleChildComponent state
      , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
      }
  where
    handleAction :: ParentAction
                 -> H.HalogenM ParentState ParentAction ChildSlots Void Aff Unit
    handleAction RandomState = do
      randInt <- liftEffect $ randomInt 1 200
      put (ParentState randInt)

    renderParentWithInputOnlyChild :: InputOnlyChildComponent -> ParentState -> H.ComponentHTML ParentAction ChildSlots Aff
    renderParentWithInputOnlyChild childComponent (ParentState inputValue) =
      HH.div_
        [ HH.div_ [ HH.text "This is the parent component " ]
        , HH.button
          [ HE.onClick \_ -> Just RandomState]
          [ HH.text "Click to send a random integer (the `input` value) \
                    \to the child"
          ]
        , HH.slot _child unit childComponent (ChildInput inputValue) (const Nothing)
        ]


data ChildInput = ChildInput Int
data ChildAction = SetState Int
data ChildState = ChildState Int

-- | A child component that renders dynamic html. It has state and
-- | responds to input, but it does not raise messages, or respond to queries.
type InputOnlyChildComponent = H.Component HH.HTML (Const Unit) ChildInput Void Aff


-- | A simple child component that only renders content to the screen
simpleChildComponent :: InputOnlyChildComponent
simpleChildComponent =
    H.mkComponent
      { initialState: \(ChildInput int) -> ChildState int
      , render: render
      , eval: H.mkEval $ H.defaultEval { handleAction = handleAction
                                       , receive = \(ChildInput i) -> Just $ SetState i
                                       }
      }
  where
    render :: ChildState -> H.ComponentHTML ChildAction () Aff
    render (ChildState state) =
      HH.div
        [ CSS.style do
            fontSize $ px 20.0
            backgroundColor orange
            padding (px 20.0) (px 20.0) (px 20.0) (px 20.0)
        ]
        [ HH.text $ "This is the child component. The input value was: " <> show state ]

    handleAction :: ChildAction
                 -> H.HalogenM ChildState ChildAction () Void Aff Unit
    handleAction (SetState i) = put (ChildState i)
