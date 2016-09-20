module Main where

import Prelude
import Kazacho (kazacho)
import UI (appInitial)
import Pux (start, renderToDOM)

main = do
  k <- kazacho
  app <- start (appInitial { initialState = k })
  renderToDOM "#app" app.html
