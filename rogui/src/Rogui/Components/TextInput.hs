{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Rogui.Components.TextInput
  ( textInput,
    handleTextInputEvent,
    handleFilteredTextInputEvent,
    defaultTextInputKeys,
    handleTextInputEvent',
  )
where

import Control.Monad (unless, when)
import Control.Monad.State.Strict
import Data.Bifunctor
import Data.Char (toUpper)
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Set as S
import Linear (V2 (..))
import Rogui.Application.Event
import Rogui.Components.Core (Component (..), DrawingContext (..), contextCellWidth, emptyComponent, recordExtent)
import Rogui.Graphics

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
        t <- gets totalElapsedTime
        drawHorizontalLine (V2 0 0) (width - 1) lightShade
        str TLeft txt
        let phase = t - fromIntegral (floor t :: Int)
        when (focused && phase < 0.7) $
          glyph fullBlock []
   in emptyComponent {draw = draw'}

data TextInputAction
  = TextInputEraseLast
  | TextInputFocusNext
  | TextInputFocusPrev
  | TextInputValidate

defaultTextInputKeys :: [(KeyMatch, TextInputAction)]
defaultTextInputKeys =
  [ (IsNoMod KBackspace, TextInputEraseLast),
    (IsNoMod KUp, TextInputFocusPrev),
    (IsNoMod KDown, TextInputFocusNext),
    (IsNoMod KEnter, TextInputFocusNext)
  ]

-- | Default implementation for text input events, with support
-- for these events:
--
-- * Backspace removes the last character;
-- * SDL TextInputEvent are used to add new characters to the text;
-- * Arrow down and up fire FocusNext / FocusPrev events.
-- * Pressing return sends next focus
handleTextInputEvent ::
  (Monad m) =>
  -- | Current text
  String ->
  -- | Function to modify the input text in your application state
  (String -> s -> s) ->
  EventHandler m s e n
handleTextInputEvent =
  handleTextInputEvent' defaultTextInputKeys Nothing

handleFilteredTextInputEvent ::
  (Monad m) =>
  -- Key input validator: only input passing this filter will be retained
  (Char -> Bool) ->
  -- | Current text
  String ->
  -- | Function to modify the input text in your application state
  (String -> s -> s) ->
  EventHandler m s e n
handleFilteredTextInputEvent validator =
  handleTextInputEvent' defaultTextInputKeys (Just validator)

handleTextInputEvent' ::
  (Monad m) =>
  -- | Mapping between keys and actions
  [(KeyMatch, TextInputAction)] ->
  -- Key input validator: only input passing this filter will be retained
  Maybe (Char -> Bool) ->
  -- | Current text
  String ->
  -- | Function to modify the input text in your application state
  (String -> s -> s) ->
  EventHandler m s e n
handleTextInputEvent' keyMap validator txt modifier =
  let onEraseLast = unless (null txt) . redraw . modifyState . modifier $ init txt
      toEvents = \case
        TextInputEraseLast -> \_ _ -> onEraseLast
        TextInputFocusNext -> \_ _ -> fireEvent $ Focus FocusNext
        TextInputFocusPrev -> \_ _ -> fireEvent $ Focus FocusPrev
        TextInputValidate -> \_ _ -> fireEvent $ Focus FocusNext
   in keyPressHandler (second toEvents <$> keyMap) <||> textInputEventHandler validator txt modifier

keyEventToChar :: KeyDetails -> Maybe Char
keyEventToChar (KeyDetails key modifier) =
  case key of
    KChar c -> Just $ if modifier == S.singleton Shift then toUpper c else c
    KPNum n -> listToMaybe $ show n
    _ -> Nothing

textInputEventHandler ::
  (Monad m) =>
  -- | Potential validator for text addition
  Maybe (Char -> Bool) ->
  -- | Current text
  String ->
  -- | Function to modify the input text in your application state
  (String -> s -> s) ->
  EventHandler m s e n
textInputEventHandler validator txt modifier _ =
  let validChar = fromMaybe (const True) validator
   in \case
        (KeyDown (KeyDownDetails _ k)) -> maybe unhandled (\c -> redraw $ modifyState (modifier $ txt <> filter validChar [c])) $ keyEventToChar k
        _ -> unhandled
