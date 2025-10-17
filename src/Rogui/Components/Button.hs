module Rogui.Components.Button
  ( button,
  )
where

import Rogui.Components.Label (label)
import Rogui.Components.Types (Component (..))
import Rogui.Graphics (TextAlign)
import Rogui.Graphics.DSL.Instructions (Colours)

button :: String -> TextAlign -> Colours -> Colours -> Bool -> Component n
button content baseAlignment normalColours focusedColours focused =
  let pickColour = if focused then focusedColours else normalColours
   in label content baseAlignment pickColour