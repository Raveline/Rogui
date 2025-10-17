module Rogui.Components.Button
  ( button,
    handleButtonEvent,
  )
where

import qualified Data.Map as M
import Rogui.Application.Event (Event (..), fireEvent)
import Rogui.Application.System (keyPressHandler)
import Rogui.Components.Label (label)
import Rogui.Components.Types (Component (..))
import Rogui.Graphics (TextAlign)
import Rogui.Graphics.DSL.Instructions (Colours)
import Rogui.Types (EventHandler)
import qualified SDL

button :: String -> TextAlign -> Colours -> Colours -> Bool -> Component n
button content baseAlignment normalColours focusedColours focused =
  let pickColour = if focused then focusedColours else normalColours
   in label content baseAlignment pickColour

-- | Fire the given event when getting enter, focus next and prev on arrows up and down
handleButtonEvent :: EventHandler state -> EventHandler state
handleButtonEvent e =
  let keyHandler =
        M.fromList
          [ (SDL.KeycodeReturn, e),
            (SDL.KeycodeUp, \_ _ -> fireEvent FocusPrev),
            (SDL.KeycodeDown, \_ _ -> fireEvent FocusNext)
          ]
   in keyPressHandler (\_ _ -> pure ()) keyHandler