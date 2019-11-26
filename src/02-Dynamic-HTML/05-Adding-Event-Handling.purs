module DynamicHtml.AddingEventHandling where

import Prelude

-- Imports for lesson
import Control.Monad.State (get, put)
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

-- Imports for scaffolding
import Data.Const (Const)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Halogen (ComponentHTML)
import Halogen as H
import Halogen.Aff (awaitBody)
import Halogen.VDom.Driver (runUI)

-- | Our state type. Either the button is 'on' or 'off'.
type State = Boolean

-- | Our action type. It indicates the button's state should be inverted
data Action = Toggle

-- | Shows how to add event handling.
toggleButton :: State -> ComponentHTML Action () Aff
toggleButton isOn =
  let toggleLabel = if isOn then "ON" else "OFF"
  in
    HH.button
      [ HE.onClick \_ -> Just Toggle ]
      [ HH.text $ "The button is " <> toggleLabel ]

-- | Shows how to use actions to update the component's state
handleAction :: Action -> H.HalogenM State Action () Void Aff Unit
handleAction = case _ of
  Toggle -> do
    oldState <- get
    let newState = not oldState
    put newState

    -- or, with one line, we could use
    -- modify_ \oldState -> not oldState


-- Now we can run the code

main :: Effect Unit
main =
  launchAff_ do
    body <- awaitBody
    runUI stateAndActionCompontent unit body

-- | Wraps Halogen types cleanly, so that one gets very clear compiler errors
stateAndActionCompontent :: H.Component HH.HTML (Const Unit) Unit Void Aff
stateAndActionCompontent =
  H.mkComponent
    { initialState: const false
    , render: toggleButton
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
