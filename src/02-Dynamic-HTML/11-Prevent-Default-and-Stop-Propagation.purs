module DynamicHtml.PreventDefault where

import Prelude

-- Imports for lesson
import Data.Maybe (Maybe(..))
import Effect.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as ME
import Web.Event.Event (preventDefault)


-- Imports for scaffolding
import Data.Const (Const)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Halogen (ComponentHTML, liftEffect)
import Halogen.Aff (awaitBody)
import Halogen.VDom.Driver (runUI)

type State = Boolean
data Action = MyMouseEvent MouseEvent

renderExample :: forall a. a -> ComponentHTML Action () Aff
renderExample _ =
  HH.div_
    [ HH.p_ [ HH.text $ "The component has no state" ]
    , HH.a
        [ HE.onClick \e -> Just (MyMouseEvent e)
        , HP.href "www.google.com"
        ]
        [ HH.text $ "Click here wont go to google" ]
    ]

handleAction :: Action -> H.HalogenM State Action () Void Aff Unit
handleAction = case _ of
  MyMouseEvent mouseEvent -> do
    liftEffect $ preventDefault $ ME.toEvent mouseEvent
    liftEffect $ log "link is click"

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
    , render: renderExample
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
