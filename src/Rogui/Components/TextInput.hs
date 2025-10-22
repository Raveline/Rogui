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
import Rogui.Application.Event (Event (..), EventHandlingM, KeyDownDetails (..), fireEvent, modifyState, redraw)
import Rogui.Components.Types (Component (..), DrawingContext (..), contextCellWidth, emptyComponent, recordExtent)
import Rogui.Graphics
import SDL (TextInputEventData (textInputEventText))
import qualified SDL

textInput :: (Ord n) => n -> String -> Colours -> Bool -> Component n
textInput n txt colours focused =
  let draw' = do
        recordExtent n
        setColours colours
        width <- contextCellWidth
        steps' <- gets steps
        drawHorizontalLine (width - 1) lightShade
        str TLeft txt
        when (focused && steps' `mod` 10 < 7) $
          glyph fullBlock
   in emptyComponent {draw = draw'}

handleTextInputEvent :: Event e -> String -> (String -> s -> s) -> EventHandlingM s e n ()
handleTextInputEvent event txt modifier = case event of
  KeyDown (KeyDownDetails {key}) ->
    case (SDL.keysymKeycode key) of
      SDL.KeycodeBackspace ->
        unless (null txt) . redraw . modifyState $ modifier (init txt)
      SDL.KeycodeUp -> fireEvent FocusPrev
      SDL.KeycodeDown -> fireEvent FocusNext
      _ -> pure ()
  OtherSDLEvent (SDL.TextInputEvent (SDL.TextInputEventData {textInputEventText})) ->
    redraw $ modifyState (modifier $ txt <> T.unpack textInputEventText)
  _ -> pure ()