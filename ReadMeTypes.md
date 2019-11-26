```purescript
import Halogen as H


H.HalogenM State Action () Void Aff Unit

H.Component HH.HTML (Const Unit) Unit Void Aff

H.ComponentHTML Action () Aff


{-

handleAction :: Action -> H.HalogenM State Action () Void Aff Unit
render :: State -> H.ComponentHTML Action () Aff
initialState :: ?? -> State
-}

H.mkComponent
    { initialState: const false
    , render: toggleButton
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

runUI :: ??

```
