module ParentChildRelationships.ChildlikeComponents.InputOnly where

import Prelude

-- Imports for lesson
import Control.Monad.State (modify_)
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP


-- Imports for scaffolding
import Control.Monad.Rec.Class (forever)
import Data.Const (Const)
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, forkAff, launchAff_)
import Effect.Random (randomInt)
import Halogen (liftEffect, put, ComponentHTML)
import Halogen as H
import Halogen.Aff (awaitBody)
import Halogen.VDom.Driver (runUI)

type State = { intValue :: Int
             , toggleState :: Boolean
             }

-- | Our component can either toggle its `toggleState`
-- | or change its `intValue`
data Action
  = Toggle
  | ReceiveParentInput Int

intToChildInitialState :: ParentState -> State
intToChildInitialState input =
  { intValue: input
  , toggleState: false
  }

childRender :: State -> ComponentHTML Action () Aff
childRender state =
  HH.div
    [ HP.class_ $ HH.ClassName "child-main-div" ]
    [ HH.text $ "The next integer is: " <> show state.intValue
    , HH.div_
      [ HH.button
        [ HE.onClick \_ -> Just Toggle ]
        [ HH.text $ "Button state: " <> show state.toggleState ]
      ]
    ]

receiveNextParentInt :: Int -> Maybe Action
receiveNextParentInt nextInputIntVal = Just $ ReceiveParentInput nextInputIntVal

handleChildAction :: Action -> H.HalogenM State Action () Void Aff Unit
handleChildAction = case _ of
  Toggle -> do
    modify_ (\oldState -> oldState { toggleState = not oldState.toggleState })
  ReceiveParentInput input -> do
    modify_ (\oldState -> oldState { intValue = input })


-- | Runs a component that converts the value of the `input` type provided
-- | by the parent (an `Int`) to a value of the `state` type as the
-- | child's initial state value, which is used render dynamic HTML
-- | with event handling via the `action` type.
main :: Effect Unit
main = do
  launchAff_ do
    body <- awaitBody
    -- firstIntVal <- liftEffect $ randomInt 1 200
    io <- runUI (parentComponent stateActionInputComponent) 120 body

    forkAff do
      forever do
        delay $ Milliseconds 2000.0
        nextIntVal <- liftEffect $ randomInt 1 200
        io.query $ H.tell $ SetState nextIntVal

-- | Wraps Halogen types cleanly, so that one gets very clear compiler errors
stateActionInputComponent :: H.Component HH.HTML (Const Unit) Int Void Aff
stateActionInputComponent =
  H.mkComponent
    { initialState: intToChildInitialState
    , render: childRender
    , eval: H.mkEval $ H.defaultEval { handleAction = handleChildAction
                                     , receive = receiveNextParentInt
                                     }
    }

type ChildComponent = H.Component HH.HTML (Const Unit) Int Void Aff

type ParentState = Int
data ParentQuery a = SetState Int a
type ParentAction = Void
type ParentComponent = H.Component HH.HTML ParentQuery Int Void Aff

_child :: SProxy "child"
_child = SProxy

parentComponent :: ChildComponent -> ParentComponent
parentComponent childComp =
    H.mkComponent
      { initialState: \is -> is + 2
      , render: parentHtml
      , eval: H.mkEval $ H.defaultEval { handleQuery = handleQuery
                                       }
      }
  where
    parentHtml :: ParentState -> H.ComponentHTML ParentAction (child :: H.Slot (Const Unit) Void Unit) Aff
    parentHtml latestInt =
      HH.div
        [ HP.class_ $ HH.ClassName "parent-main-div" ]
        [ HH.slot _child unit childComp latestInt (const Nothing)
        ]

    handleQuery :: forall a. ParentQuery a
                -> H.HalogenM ParentState ParentAction (child :: H.Slot (Const Unit) Void Unit) Void Aff (Maybe a)
    handleQuery (SetState nextInt next) = do
      put nextInt
      pure $ Just next
