{-# LANGUAGE FlexibleContexts #-}

-- | The simple DSL defined in `Graphics.DSL.Instructions` gets evaluated
-- in this module. Implementation details are not exposed, but here's a
-- quick summary: we create a drawing state to track console, brush,
-- pencil position and current colours. Instructions are followed in
-- their declaration order. Each drawing instruction creates a SDL
-- call. This logic is not optimized to batch calls, mostly because
-- we have yet to run in actual performance issues - but some potential
-- optimisations could be added in future versions if the need arise.
module Rogui.Backend.SDL.Eval
  ( evalSDLInstructions,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State (MonadState, evalStateT, get, gets, modify)
import Data.Char (ord)
import Data.Foldable
import qualified Data.Map.Strict as M
import Data.Maybe
import Linear
import Rogui.Backend.SDL.Primitives (RGBA, clipToConsole, fillConsoleWith, overlayRect, printCharAt, setFrontColour)
import Rogui.Graphics.Colours (Colours (..))
import Rogui.Graphics.Constants
import Rogui.Graphics.DSL.Instructions (Instruction (..), Instructions, TextAlign (..))
import Rogui.Graphics.Types (Brush (..), Cell (..), Console (Console, height, width), (./.=))
import SDL (Renderer, Texture)

data DrawingState = DrawingState
  { console :: !Console,
    brush :: !Brush,
    texture :: !Texture,
    position :: !(V2 Cell),
    renderer :: !Renderer,
    colours :: !Colours,
    lastAppliedFront :: !(Maybe RGBA)
  }

-- | Using the given render, default console and default brush, apply a set
-- of instructions.
evalSDLInstructions :: (MonadIO m) => Renderer -> M.Map Brush SDL.Texture -> Console -> Brush -> Instructions -> m ()
evalSDLInstructions renderer knownTextures console brush instructions =
  let position = V2 0 0
      colours = Colours {front = Nothing, back = Nothing}
      lastAppliedFront = Nothing
      texture = fromMaybe (error $ "Unknown texture for " <> show brush) $ brush `M.lookup` knownTextures
   in evalStateT (traverse_ (eval knownTextures) instructions) (DrawingState {..})

applyFrontColourIfNeeded ::
  (MonadState DrawingState m, MonadIO m) =>
  Maybe RGBA -> m ()
applyFrontColourIfNeeded newColour = do
  lastColour <- gets lastAppliedFront
  currentTexture <- gets texture
  when (lastColour /= newColour) $ do
    traverse_ (setFrontColour currentTexture) newColour
    modify (\s -> s {lastAppliedFront = newColour})

eval :: (MonadState DrawingState m, MonadIO m) => M.Map Brush SDL.Texture -> Instruction -> m ()
eval textures instruction = do
  DrawingState {..} <- get
  let Colours {..} = colours
  case instruction of
    OnConsole newConsole -> do
      modify (\s -> s {console = newConsole})
      clipToConsole renderer newConsole
    WithBrush newBrush ->
      let newTexture = fromMaybe (error $ "Unknown texture for " <> show newBrush) $ newBrush `M.lookup` textures
       in modify (\s -> s {brush = newBrush, lastAppliedFront = Nothing, texture = newTexture})
    DrawBorder -> do
      void $ applyFrontColourIfNeeded front
      let Console {..} = console
          Brush {..} = brush
          (w, h) = (width ./.= tileWidth - 1, height ./.= tileHeight - 1)
          draw = printCharAt renderer console brush texture [] back
          bottoms = [V2 x y | x <- [1 .. w - 1], y <- [h]]
          tops = [V2 x y | x <- [1 .. w - 1], y <- [0]]
          lefts = [V2 x y | x <- [0], y <- [1 .. h - 1]]
          rights = [V2 x y | x <- [w], y <- [1 .. h - 1]]
      traverse_ (draw horizontal437) tops
      traverse_ (draw horizontal437) bottoms
      traverse_ (draw vertical437) lefts
      traverse_ (draw vertical437) rights
      void $ draw cornerTopLeft437 $ V2 0 0
      void $ draw cornerBottomLeft437 $ V2 0 h
      void $ draw cornerTopRight437 $ V2 w 0
      void $ draw cornerBottomRight437 $ V2 w h
    DrawString alignment str -> do
      applyFrontColourIfNeeded front
      let draw n = printCharAt renderer console brush texture [] back (ord n)
          basePos = case alignment of
            TCenter -> position - V2 (Cell $ length str `div` 2) 0
            TRight -> position - V2 (Cell $ length str) 0
            _ -> position
          next (i, c) = draw c (basePos + (Cell <$> V2 1 0 ^* i))
          indexed = zip [0 ..] str
       in traverse_ next indexed
    SetConsoleBackground rgb ->
      fillConsoleWith renderer console rgb
    NewLine ->
      let (V2 px _) = position
       in modify (\s -> s {position = position + V2 (-px) 1})
    DrawGlyph glyphId trans -> do
      applyFrontColourIfNeeded front
      printCharAt renderer console brush texture trans back glyphId position
    MoveTo pos ->
      modify (\s -> s {position = pos})
    MoveBy by ->
      modify (\s -> s {position = position + by})
    SetColours col ->
      modify (\s -> s {colours = col})
    OverlayAt at colour mode ->
      overlayRect renderer console brush at (V2 1 1) colour mode
    DrawGlyphAts ats glyphId -> do
      void $ applyFrontColourIfNeeded front
      traverse_ (printCharAt renderer console brush texture [] back glyphId) ats
    FullConsoleOverlay colour mode ->
      overlayRect renderer console brush (V2 0 0) (V2 (width console ./.= tileWidth brush) (height console ./.= tileHeight brush)) colour mode