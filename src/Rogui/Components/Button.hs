module Rogui.Components.Button
  ( button,
    handleButtonEvent,
    ButtonAction (..),
  )
where

import Rogui.Application.Event
import Rogui.Components.Core (Component (..), recordExtent)
import Rogui.Components.Label (label)
import Rogui.Graphics (Colours, TextAlign)
import qualified SDL

-- | A simple button with some text, and some options when focused.
-- N.B.: this expects a `Name`, as we will record the extent, to support
-- mouse clicks on this component.
--
-- @
-- data Names = BigRedButton
--
-- renderButton :: FocusRing -> Component Name
-- renderButton fr =
--   button
--     BigRedButton
--     "Do not press !"
--     TLeft (Colours (Just white) (Just black))
--     (Colours (Just black) (Just white))
--     (focusGetCurrent fr == Just BigRedButton)
button ::
  (Ord n) =>
  n ->
  -- Name of the component.
  String ->
  -- Text content
  TextAlign ->
  -- Text alignment
  Colours ->
  -- Colours when not focused
  Colours ->
  -- Colours when focused
  Bool ->
  -- Is the component focused ?
  Component n
button n content baseAlignment normalColours focusedColours focused =
  let pickColour = if focused then focusedColours else normalColours
      labelComponent = label content baseAlignment pickColour
   in labelComponent
        { draw = recordExtent n >> draw labelComponent
        }

-- | The possible actions with a button
data ButtonAction
  = -- | Distribute focus
    ButtonNextFocus
  | -- | Distribute focus
    ButtonPrevFocus
  | -- | Activate the button
    ButtonFire

-- | A sensible default implementation for the button component.
-- This will fire the given event when getting enter, and
-- focus next and prev on arrows up and down.
handleButtonEvent :: (Monad m) => Event e -> EventHandler m state e n
handleButtonEvent toFire =
  let keyHandler =
        [ (isSC SDL.ScancodeReturn mempty, \_ _ -> fireEvent toFire),
          (isSC SDL.ScancodeDown mempty, \_ _ -> fireEvent $ Focus FocusPrev),
          (isSC SDL.ScancodeUp mempty, \_ _ -> fireEvent $ Focus FocusNext)
        ]
   in keyPressHandler keyHandler