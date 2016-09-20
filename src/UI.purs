module UI where

import Prelude
import Pux (EffModel, Config, noEffects)
import Pux.Html (Html, div, a, text, h1)
import Pux.Html.Attributes (className)
import Pux.Html.Events (onClick)
import Control.Monad.Aff
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Random (random, RANDOM)
import Kazacho (kazacho)

type State = String

data Action = GenNext | GotNext String

update :: Action -> State -> EffModel State Action (random :: RANDOM)
update (GotNext s) _ = noEffects $ s
update GenNext _ =
  { state: "Генерируем"
  , effects: [do
      k <- liftEff $ kazacho
      pure $ GotNext k
    ]
  }

btn :: Html Action
btn =
  div
    [className "center-align"]
    [a
      [ className "waves-effect waves-light btn-large"
      , onClick (const GenNext)
      ]
      [text "Любо!"]]

container :: Array (Html Action) -> Html Action
container children = div [className "container"] children

row :: Array (Html Action) -> Html Action
row children = div [className "row"] children

txt :: State -> Html Action
txt state =
  div
    [className "valign-wrapper main-content"]
    [div [className "valign center-align"] [h1 [] [text state]]]

view :: State -> Html Action
view state =
  container [ row [txt state], row [btn] ]

appInitial =
  { initialState: ""
  , update: update
  , view: view
  , inputs: []
  }
