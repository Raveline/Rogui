{-# LANGUAGE LambdaCase #-}

module Rogui.Components.Button
  ( button,
    handleButtonEvent,
    handleButtonEvent',
    defaultButtonKeys,
    ButtonAction (..),
  )
where

import Data.Bifunctor
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
    ButtonFocusNext
  | -- | Distribute focus
    ButtonFocusPrev
  | -- | Activate the button
    ButtonFire

-- | The default managed keys for buttons:
-- * Enter to activate the button;
-- * Up to focus previous;
-- * Down to focus next;
defaultButtonKeys :: [(KeyDetailsMatch, ButtonAction)]
defaultButtonKeys =
  [ (isSC' SDL.ScancodeReturn, ButtonFire),
    (isSC' SDL.ScancodeDown, ButtonFocusNext),
    (isSC' SDL.ScancodeUp, ButtonFocusPrev)
  ]

-- | A sensible default implementation for the button component.
-- This will fire the given event when getting enter, and
-- focus next and prev on arrows up and down.
handleButtonEvent :: (Monad m) => Event e -> EventHandler m state e n
handleButtonEvent = handleButtonEvent' defaultButtonKeys

-- | A version of handle button event that lets you override the
-- default keys.
handleButtonEvent' :: (Monad m) => [(KeyDetailsMatch, ButtonAction)] -> Event e -> EventHandler m state e n
handleButtonEvent' keysToActions toFire =
  let toEvents = \case
        ButtonFire -> \_ _ -> fireEvent toFire
        ButtonFocusNext -> \_ _ -> fireEvent $ Focus FocusNext
        ButtonFocusPrev -> \_ _ -> fireEvent $ Focus FocusPrev
   in keyPressHandler (second toEvents <$> keysToActions)