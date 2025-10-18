{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Rogui.Components.TextInput
  ( textInput,
    handleTextInputEvent,
  )
where

import Control.Monad (unless)
import qualified Data.Text as T
import Rogui.Application.Event (Event (..), EventHandlingM, KeyDownDetails (..), fireEvent, modifyState, redraw)
import Rogui.Components.Types (Component (..), DrawingContext (..), emptyComponent)
import Rogui.Graphics
import SDL (TextInputEventData (textInputEventText))
import qualified SDL

textInput :: String -> Colours -> Bool -> Component n
textInput txt colours _focused =
  let draw' DrawingContext {..} = do
        let Console {..} = console
            TileSize {..} = tileSize
        setColours colours
        drawLine (SDL.V2 (width * pixelWidth) 0)
        strLn TLeft txt
   in emptyComponent {draw = draw'}

handleTextInputEvent :: Event e -> String -> (String -> s -> s) -> EventHandlingM s e ()
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