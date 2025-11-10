{-# LANGUAGE FlexibleContexts #-}

-- | The simple DSL defined in `Graphics.DSL.Instructions` gets evaluated
-- in this module. Implementation details are not exposed, but here's a
-- quick summary: we create a drawing state to track console, brush,
-- pencil position and current colours. Instructions are followed in
-- their declaration order. Each drawing instruction creates a SDL
-- call. This logic is not optimized to batch calls, mostly because
-- we have yet to run in actual performance issues - but some potential
-- optimisations could be added in future versions if the need arise.
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
import Rogui.Graphics.Types (Brush (tileHeight, tileWidth), Cell (..), Console (height, width), (./.=))
import SDL (Renderer, V2 (..))

data DrawingState = DrawingState
  { console :: Console,
    brush :: Brush,
    position :: V2 Cell,
    renderer :: Renderer,
    colours :: Colours
  }

-- | Using the given render, default console and default brush, apply a set
-- of instructions.
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
    OverlayAt at colour ->
      overlayRect renderer console brush at (V2 1 1) colour
    DrawGlyphAts ats glyphId ->
      traverse_ (printCharAt renderer console brush [] front back glyphId) ats
    FullConsoleOverlay colour ->
      overlayRect renderer console brush (V2 0 0) (V2 (width console ./.= tileWidth brush) (height console ./.= tileHeight brush)) colour