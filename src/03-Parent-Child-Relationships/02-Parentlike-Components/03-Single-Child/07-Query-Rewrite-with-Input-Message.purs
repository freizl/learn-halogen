module ParentChildRelationships.ParentlikeComponents.SingleChild.QueryRewriteWithInputMessage where

import Prelude

-- Imports for lesson
import Control.Monad.State (put, get, modify_)
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Random (randomInt)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

-- Imports for scaffolding
import Data.Const (Const)
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff, launchAff_)
import Halogen.Aff (awaitBody)
import Halogen.VDom.Driver (runUI)


main :: Effect Unit
main = do
  launchAff_ do
    body <- awaitBody
    runUI parentComponent unit body

-- We'll store the child's state. Since the parent component might not
-- know what that is when it initially renders, we'll wrap it in a Maybe.
type ParentState =
  { childState :: Maybe Int
  , childInput :: Maybe ChildInput
  }

data ParentAction =
    InputToChild ChildInput
  | ChildMessage Int


parentComponent :: H.Component HH.HTML (Const Void) Unit Void Aff
parentComponent =
    H.mkComponent
    { initialState: \_ -> { childState : Nothing, childInput : Just GetChildState }
    , render: render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
    render :: ParentState -> H.ComponentHTML ParentAction ChildSlots Aff
    render state =
      HH.div_
        [ HH.div_ [ HH.text $ "The child's state is: " <> (maybe "<unknown>" show state.childState) ]
        , HH.div_
          [ HH.button
            [ HE.onClick \_ -> Just (InputToChild GetChildState) ]
            [ HH.text "Get child state" ]
          ]
        , HH.div_
          [ HH.button
            [ HE.onClick \_ -> Just (InputToChild (SetChildState 13)) ]
            [ HH.text "Set child state to  random integer and \
                      \clear parent's memory of child state"
            ]
          ]
        , HH.div_
          [ HH.button
            [ HE.onClick \_ -> Just (InputToChild (SetAndGetDoubledChildState 23)) ]
            [ HH.text "Set child state to a random integer and \
                      \store (newState * 2) as parent's memory \
                      \of child state"
            ]
          ]
        , HH.div_
          [ HH.slot _child unit childComp (state.childInput) (\msg -> Just $ ChildMessage msg) ]
        ]

    handleAction :: ParentAction -> H.HalogenM ParentState ParentAction ChildSlots Void Aff Unit
    handleAction = case _ of
      InputToChild toChild -> modify_ (\oldState -> oldState { childInput = Just toChild })  -- could do pattern matching and generates random value
      ChildMessage msg -> modify_ (\oldState -> oldState { childState = Just msg, childInput = Nothing })


-- Child component

type ChildSlots = (child :: H.Slot (Const Unit) Int Unit)
_child :: SProxy "child"
_child = SProxy

type ChildComponent = H.Component HH.HTML (Const Unit) (Maybe ChildInput) Int Aff

data ChildInput
  = GetChildState
  | SetChildState Int
  | SetAndGetDoubledChildState Int
data ChildAction = HandleInput ChildInput

childComp :: ChildComponent
childComp =
    H.mkComponent
      { initialState
      , render
      , eval: H.mkEval $ H.defaultEval { receive = receive, handleAction = handleAction }
      }
  where
    initialState :: (Maybe ChildInput) -> Int
    initialState _ = 42

    render :: Int -> H.ComponentHTML ChildAction () Aff
    render state =
      HH.div_
        [ HH.text $ "Child state: " <> show state ]

    receive :: Maybe ChildInput -> Maybe ChildAction
    receive (Just msg) = Just $ HandleInput msg
    receive Nothing = Nothing

    handleAction :: ChildAction
                 -> H.HalogenM Int ChildAction () Int Aff Unit
    handleAction (HandleInput childInput) = do
        state <- get
        case childInput of
          GetChildState -> H.raise state
          SetChildState newState -> do
            modify_ (\oldState -> newState)
          SetAndGetDoubledChildState newState -> do
            modify_ (\oldState -> newState)
            H.raise (newState * 2)

    -- handleQuery :: forall a.
    --                ChildQuery a
    --             -> H.HalogenM Int Void () Void Aff (Maybe a)
    -- handleQuery = case _ of
    --   GetState reply -> do
    --     state <- get
    --     pure $ Just $ reply state
    --   SetState state next -> do
    --     put state
    --     pure $ Just next
    --   SetAndGetDoubledState state reply -> do
    --     put state
    --     let doubledState = state * 2
    --     pure $ Just $ reply doubledState
