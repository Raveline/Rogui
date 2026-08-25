{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}

-- | Canvas 2D interpreter for `Rogui.Graphics.DSL.Instructions`, mirroring
-- `Rogui.Backend.SDL.Eval`'s state machine (console/brush/pencil/colours)
-- but issuing `drawImage`/`fillRect`/`clip` calls (via
-- `Rogui.Backend.WASM.Primitives`) instead of SDL ones.
--
-- One structural difference from the SDL version: SDL applies the
-- foreground colour as texture-mutating state before each draw (and skips
-- re-applying it when unchanged, as an optimisation). Canvas 2D draws are
-- stateless, so here the current foreground colour is simply threaded
-- through and passed to every `printCharAt` call directly.
module Rogui.Backend.WASM.Eval
  ( evalWASMInstructions,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (MonadState, evalStateT, get, modify)
import Data.Char (ord)
import Data.Foldable (traverse_)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Linear (V2 (..), (^*))
import Rogui.Backend.WASM.FFI (CanvasContext, WASMTexture)
import Rogui.Backend.WASM.Primitives (clipToConsole, fillConsoleWith, overlayRect, printCharAt)
import Rogui.Graphics.Colours (Colours (..))
import Rogui.Graphics.Constants
import Rogui.Graphics.DSL.Instructions (Instruction (..), Instructions, TextAlign (..))
import Rogui.Graphics.Types (Brush (..), Cell (..), Console (Console, height, width), (./.=))

data DrawingState = DrawingState
  { console :: !Console,
    brush :: !Brush,
    texture :: !WASMTexture,
    position :: !(V2 Cell),
    ctx :: !CanvasContext,
    colours :: !Colours
  }

-- | Using the given context, default console and default brush, apply a
-- set of instructions.
evalWASMInstructions ::
  (MonadIO m) =>
  CanvasContext ->
  M.Map Brush WASMTexture ->
  Console ->
  Brush ->
  Instructions ->
  m ()
evalWASMInstructions ctx knownTextures console brush instructions =
  let position = V2 0 0
      colours = Colours {front = Nothing, back = Nothing}
      texture = fromMaybe (error $ "Unknown texture for " <> show brush) $ brush `M.lookup` knownTextures
   in evalStateT (traverse_ (eval knownTextures) instructions) (DrawingState {..})

eval :: (MonadState DrawingState m, MonadIO m) => M.Map Brush WASMTexture -> Instruction -> m ()
eval textures instruction = do
  DrawingState {..} <- get
  let Colours {..} = colours
  case instruction of
    OnConsole newConsole -> do
      modify (\s -> s {console = newConsole})
      clipToConsole ctx newConsole
    WithBrush newBrush ->
      let newTexture = fromMaybe (error $ "Unknown texture for " <> show newBrush) $ newBrush `M.lookup` textures
       in modify (\s -> s {brush = newBrush, texture = newTexture})
    DrawBorder -> do
      let Console {..} = console
          Brush {..} = brush
          (w, h) = (width ./.= tileWidth - 1, height ./.= tileHeight - 1)
          draw = printCharAt ctx console brush texture [] front back
          bottoms = [V2 x y | x <- [1 .. w - 1], y <- [h]]
          tops = [V2 x y | x <- [1 .. w - 1], y <- [0]]
          lefts = [V2 x y | x <- [0], y <- [1 .. h - 1]]
          rights = [V2 x y | x <- [w], y <- [1 .. h - 1]]
      traverse_ (draw horizontal437) tops
      traverse_ (draw horizontal437) bottoms
      traverse_ (draw vertical437) lefts
      traverse_ (draw vertical437) rights
      draw cornerTopLeft437 $ V2 0 0
      draw cornerBottomLeft437 $ V2 0 h
      draw cornerTopRight437 $ V2 w 0
      draw cornerBottomRight437 $ V2 w h
    DrawString alignment str -> do
      let drawChar c = printCharAt ctx console brush texture [] front back (ord c)
          basePos = case alignment of
            TCenter -> position - V2 (Cell $ length str `div` 2) 0
            TRight -> position - V2 (Cell $ length str) 0
            _ -> position
          next (i, c) = drawChar c (basePos + (Cell <$> V2 1 0 ^* i))
          indexed = zip [0 ..] str
       in traverse_ next indexed
    SetConsoleBackground rgb ->
      fillConsoleWith ctx console rgb
    NewLine ->
      let (V2 px _) = position
       in modify (\s -> s {position = position + V2 (-px) 1})
    DrawGlyph glyphId trans ->
      printCharAt ctx console brush texture trans front back glyphId position
    MoveTo pos ->
      modify (\s -> s {position = pos})
    MoveBy by ->
      modify (\s -> s {position = position + by})
    SetColours col ->
      modify (\s -> s {colours = col})
    OverlayAt at colour mode ->
      overlayRect ctx console brush at (V2 1 1) colour mode
    DrawGlyphAts ats glyphId ->
      traverse_ (printCharAt ctx console brush texture [] front back glyphId) ats
    FullConsoleOverlay colour mode ->
      overlayRect ctx console brush (V2 0 0) (V2 (width console ./.= tileWidth brush) (height console ./.= tileHeight brush)) colour mode
