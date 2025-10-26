module Rogui.Components.Button
  ( button,
    handleButtonEvent,
  )
where

import qualified Data.Map as M
import Rogui.Application.Event (Event (..), fireEvent)
import Rogui.Application.System (keyPressHandler)
import Rogui.Components.Label (label)
import Rogui.Components.Types (Component (..), recordExtent)
import Rogui.Graphics (Colours, TextAlign)
import Rogui.Types (EventHandler)
import qualified SDL

button :: (Ord n) => n -> String -> TextAlign -> Colours -> Colours -> Bool -> Component n
button n content baseAlignment normalColours focusedColours focused =
  let pickColour = if focused then focusedColours else normalColours
      labelComponent = label content baseAlignment pickColour
   in labelComponent
        { draw = recordExtent n >> draw labelComponent
        }

-- | Fire the given event when getting enter, focus next and prev on arrows up and down
handleButtonEvent :: Event e -> EventHandler state e n
handleButtonEvent toFire =
  let keyHandler =
        M.fromList
          [ ((SDL.KeycodeReturn, mempty), \_ _ -> fireEvent toFire),
            ((SDL.KeycodeUp, mempty), \_ _ -> fireEvent FocusPrev),
            ((SDL.KeycodeDown, mempty), \_ _ -> fireEvent FocusNext)
          ]
   in keyPressHandler keyHandler