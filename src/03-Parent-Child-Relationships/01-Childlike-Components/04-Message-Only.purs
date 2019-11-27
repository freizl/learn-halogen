module ParentChildRelationships.ChildlikeComponents.MessageOnly where

import Prelude

-- Imports for lesson
import Control.Monad.State (get, modify_)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

-- Imports for scaffolding
import CSS (em, marginTop, border, solid, px, red)
import Data.Array ((:))
import Data.Const (Const)
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Halogen (ComponentHTML)
import Halogen.Aff (awaitBody)
import Halogen.HTML.CSS as CSS
import Halogen.VDom.Driver (runUI)

type State = Int

data Action
  = Increment
  | Decrement
  | NotifyParentAboutState
  | NotifyParentTextMessage String

type Message = String

render :: State -> ComponentHTML Action () Aff
render counterState =
  let yourMessage = "message from child"
  in
    HH.div
      [ HP.class_ $ HH.ClassName "child-main-div"
      , CSS.style do
          border solid (px 2.0) red
      ]
      [ HH.div_
        [ HH.button
          [ HE.onClick \_ -> Just NotifyParentAboutState ]
          [ HH.text "Log current counter value" ]
        ]
      , HH.div_
        [ HH.button
          [ HE.onClick \_ -> Just Decrement ]
          [ HH.text $ "-"]
        , HH.button
          [ HE.onClick \_ -> Just Increment ]
          [ HH.text $ "+"]
        ]
      , HH.div_ [ HH.text $ show counterState ]
      , HH.div_
        [ HH.button
          [ HE.onClick \_ -> Just $ NotifyParentTextMessage yourMessage ]
          [ HH.text $ "Log '" <> yourMessage <> "' to the page."]]
      ]


handleAction :: Action -> H.HalogenM State Action () String Aff Unit
handleAction = case _ of
  Increment -> do
    modify_ (\oldState -> oldState + 1)
  Decrement -> do
    modify_ (\oldState -> oldState - 1)
  NotifyParentAboutState -> do
    currentState <- get
    H.raise $ show currentState
  NotifyParentTextMessage message -> do
    H.raise $ message

-- | Runs a component that converts the value of the `input` type provided
-- | by the parent (an `Int`) to a value of the `state` type as the
-- | child's initial state value, which is used to render dynamic HTML
-- | with event handling via the `action` type.
main :: Effect Unit
main = do
  launchAff_ do
    body <- awaitBody
    runUI (parentComponent stateActionMessageComponent) unit body

type ChildComponent = H.Component HH.HTML (Const Unit) Unit String Aff

-- | Wraps Halogen types cleanly, so that one gets very clear compiler errors
stateActionMessageComponent :: ChildComponent
stateActionMessageComponent =
  H.mkComponent
    { initialState: const 0
    , render: render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

data ParentAction = AddMessage String
type ParentState = Array String
type ParentQuery = Const Unit
type ParentComponent = H.Component HH.HTML ParentQuery Unit Void Aff

_child :: SProxy "child"
_child = SProxy

type ChildSlots = ( child :: H.Slot (Const Unit) String Unit )

parentComponent :: ChildComponent -> ParentComponent
parentComponent childComp =
    H.mkComponent
      { initialState: const []
      , render: parentRender
      , eval: H.mkEval $ H.defaultEval { handleAction = handleParentAction }
      }
  where
    parentRender :: ParentState -> H.ComponentHTML ParentAction ChildSlots Aff
    parentRender logArray =
      HH.div_
        [ HH.div_
          [ HH.text "Use your child component's html to send messages to \
                    \the parent. It will render the message below them all"
          ]
        , HH.slot _child unit childComp unit (\msg -> Just $ AddMessage msg)
        , HH.div
          [ CSS.style do
            marginTop $ em 3.0
          ]
          (logArray <#> \log ->
            HH.div_ [ HH.text $ "Message received: " <> log]
          )
        ]

    handleParentAction
      :: ParentAction
      -> H.HalogenM ParentState ParentAction ChildSlots Void Aff Unit
    handleParentAction (AddMessage msg) = do
      -- add the message to the front of the array
      modify_ (\array -> msg : array)
