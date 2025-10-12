{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Rogui.Graphics.DSL.Eval
  ( evalInstructions,
  )
where

import Control.Monad.IO.Class
import Control.Monad.State (MonadState, evalStateT, get, modify)
import Data.Foldable
import Rogui.Graphics.Console (drawBorder, printStrAt)
import Rogui.Graphics.DSL.Instructions (Colours (..), Instruction (..), Instructions)
import Rogui.Graphics.Primitives (printCharAt)
import Rogui.Graphics.Types (Brush, Console)
import SDL (Renderer, V2 (..))

data DrawingState = DrawingState
  { console :: Console,
    brush :: Brush,
    position :: V2 Int,
    renderer :: Renderer,
    colours :: Colours
  }

evalInstructions :: (MonadIO m) => Renderer -> Console -> Brush -> Instructions -> m ()
evalInstructions renderer console brush instructions =
  let position = V2 0 0
      colours = Colours {front = Nothing, back = Nothing}
   in evalStateT (traverse_ eval instructions) (DrawingState {..})

eval :: (MonadState DrawingState m, MonadIO m) => Instruction -> m ()
eval instruction = do
  DrawingState {..} <- get
  let Colours {..} = colours
  case instruction of
    OnConsole newConsole ->
      modify (\s -> s {console = newConsole})
    WithBrush newBrush ->
      modify (\s -> s {brush = newBrush})
    DrawBorder -> drawBorder renderer console brush front back
    DrawString align str ->
      printStrAt renderer console brush front back align str position
    NewLine ->
      modify (\s -> s {position = position + (V2 0 1)})
    DrawGlyph glyphId ->
      printCharAt renderer console brush front back glyphId position
    MoveTo pos ->
      modify (\s -> s {position = pos})
    SetColours col ->
      modify (\s -> s {colours = col})