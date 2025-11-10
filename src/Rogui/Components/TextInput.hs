{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Rogui.Components.TextInput
  ( textInput,
    handleTextInputEvent,
  )
where

import Control.Monad (unless, when)
import Control.Monad.State.Strict
import qualified Data.Text as T
import Rogui.Application.Event (Event (..), EventHandlerM, KeyDetails (..), KeyDownDetails (..), fireEvent, modifyState, redraw, unhandled)
import Rogui.Components.Core (Component (..), DrawingContext (..), contextCellWidth, emptyComponent, recordExtent)
import Rogui.Graphics
import SDL (TextInputEventData (textInputEventText))
import qualified SDL
import SDL.Vect (V2 (..))

-- | A single line text input component. It assumes you are using a CCSID 437
-- charset, with a light shade and full block glyph id. If you don't, you'll
-- have to roll your own implementation. The component records its extent so you
-- can set focus to it on click.
textInput ::
  (Ord n) =>
  -- | Name of the component
  n ->
  -- | Current input
  String ->
  -- | Colours to use
  Colours ->
  -- | Is the component focused ?
  Bool ->
  Component n
textInput n txt colours focused =
  let draw' = do
        recordExtent n
        setColours colours
        width <- contextCellWidth
        steps' <- gets steps
        drawHorizontalLine (V2 0 0) (width - 1) lightShade
        str TLeft txt
        when (focused && steps' `mod` 10 < 7) $
          glyph fullBlock []
   in emptyComponent {draw = draw'}

-- | Default implementation for text input events, with support
-- for these events:
--
-- * Backspace removes the last character;
-- * SDL TextInputEvent are used to add new characters to the text;
-- * Arrow down and up fire FocusNext / FocusPrev events.
handleTextInputEvent ::
  (Monad m) =>
  -- | Event to process
  Event e ->
  -- | Current text
  String ->
  -- | Function to modify the input text in your application state
  (String -> s -> s) ->
  EventHandlerM m s e n ()
handleTextInputEvent event txt modifier = case event of
  KeyDown KeyDownDetails {key} ->
    case keycode key of
      SDL.KeycodeBackspace ->
        unless (null txt) . redraw . modifyState . modifier $ init txt
      SDL.KeycodeUp -> fireEvent FocusPrev
      SDL.KeycodeDown -> fireEvent FocusNext
      _ -> unhandled
  OtherSDLEvent (SDL.TextInputEvent SDL.TextInputEventData {textInputEventText}) ->
    redraw $ modifyState (modifier $ txt <> T.unpack textInputEventText)
  _ -> unhandled