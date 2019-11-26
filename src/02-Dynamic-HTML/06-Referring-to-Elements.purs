module DynamicHtml.ReferringToElements where

import Prelude

-- Imports for lesson
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect.Console (log)
-- import Halogen ()
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE

-- Imports for scaffolding
import Data.Const (Const)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Halogen (ComponentHTML, liftEffect)
import Halogen.Aff (awaitBody)
import Halogen.VDom.Driver (runUI)

type State = Boolean
data Action = PrintExample

renderExample :: State -> ComponentHTML Action () Aff
renderExample state =
  HH.div_
    [ HH.p_ [ HH.text $ "The state of the component is: " <> show state ]
    , HH.button
        [ HE.onClick \_ -> Just PrintExample
        -- here, we label this button as 'my-button', so we can refer to it later
        , HP.ref (H.RefLabel "my-button")
        ]
        [ HH.text $ "Click to print our example to the console." ]
    ]

handleAction :: Action -> H.HalogenM State Action () Void Aff Unit
handleAction = case _ of
  PrintExample -> do
    -- Here, we use this reference to do something with the element
    H.getHTMLElementRef (H.RefLabel "my-button") >>= traverse_ \element -> do
      -- in this situation, we'll just state that the element exists
      -- and could be used here
      liftEffect $ log "We can now do something directly to the \
                         \button HTML element."

-- Now we can run the code

main :: Effect Unit
main =
  launchAff_ do
    body <- awaitBody
    runUI (stateAndActionCompontent) unit body

-- | Wraps Halogen types cleanly, so that one gets very clear compiler errors
stateAndActionCompontent :: H.Component HH.HTML (Const Unit) Unit Void Aff
stateAndActionCompontent =
  H.mkComponent
    { initialState: const false
    , render: renderExample
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
