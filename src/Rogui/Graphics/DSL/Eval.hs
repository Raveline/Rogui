{-# LANGUAGE FlexibleContexts #-}

module Rogui.Graphics.DSL.Eval
  ( evalInstructions,
  )
where

import Control.Monad.IO.Class
import Control.Monad.State (MonadState, evalStateT, get, modify)
import Data.Foldable
import Rogui.Graphics.Colours (Colours (..))
import Rogui.Graphics.Console (drawBorder, printStrAt)
import Rogui.Graphics.DSL.Instructions (Instruction (..), Instructions)
import Rogui.Graphics.Primitives (clipToConsole, fillConsoleWith, overlayRect, printCharAt)
import Rogui.Graphics.Types (Brush, Cell (..), Console)
import SDL (Renderer, V2 (..))

data DrawingState = DrawingState
  { console :: Console,
    brush :: Brush,
    position :: V2 Cell,
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
    OnConsole newConsole -> do
      modify (\s -> s {console = newConsole})
      clipToConsole renderer newConsole
    WithBrush newBrush ->
      modify (\s -> s {brush = newBrush})
    DrawBorder -> drawBorder renderer console brush front back
    DrawString align str ->
      printStrAt renderer console brush front back align str position
    SetConsoleBackground rgb ->
      fillConsoleWith renderer console rgb
    NewLine ->
      let (V2 px _) = position
       in modify (\s -> s {position = position + V2 (-px) 1})
    DrawGlyph glyphId trans ->
      printCharAt renderer console brush trans front back glyphId position
    MoveTo pos ->
      modify (\s -> s {position = pos})
    MoveBy by ->
      modify (\s -> s {position = position + by})
    SetColours col ->
      modify (\s -> s {colours = col})
    DrawHorizontalLine to glyph ->
      let (V2 fromX fromY) = position
          cells = [V2 x y | x <- [fromX .. to], y <- [fromY]]
       in traverse_ (printCharAt renderer console brush [] front back glyph) cells
    OverlayAt at colour ->
      overlayRect renderer console brush at (V2 1 1) colour