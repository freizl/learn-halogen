## Types

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

runUI :: H.Component ... -> ParameterForinitialState -> DomContainer


HH.slot _child    unit childComponent   unit                 (\msg -> Just $ AddMessage msg)
        proxy??   ??                    child init state     raise child message to parent (Action)


H.Slot (Const Unit) Int Unit
        ??          ??  ??
```

## Questions

- `handleQuery` like magic. how does it send value to child component?
