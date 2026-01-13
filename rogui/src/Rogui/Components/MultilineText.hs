{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | A simple multiline text component with word wrapping support.
--
-- This component displays multi-colored text that wraps to fit within
-- the available width. For scrolling support, compose with
-- `Rogui.Components.Viewport.viewportClipped`.
--
-- @
-- -- Simple usage:
-- renderDescription :: Component n
-- renderDescription = multilineText
--   [ (Colours (Just white) (Just black), "This is a long text that will wrap")
--   , (Colours (Just red) Nothing, "automatically when it exceeds")
--   , (Colours (Just white) (Just black), "the available width.")
--   ]
--
-- -- With scrolling:
-- import Rogui.Components.Viewport (viewportClipped, ViewportState (..))
--
-- renderScrollableText :: ViewportState -> Component n
-- renderScrollableText viewportState =
--   viewportClipped TextView viewportState $
--     multilineText
--       [ (Colours (Just white) (Just black), "Long text here...")
--       , (Colours (Just red) Nothing, "More text...")
--       ]
-- @
module Rogui.Components.MultilineText
  ( multilineText,
    TextChunk,
    splitChunksByNewlines,
  )
where

import Control.Monad (foldM)
import Control.Monad.State.Strict (get, modify)
import qualified Data.Map as M
import Linear (V2 (..))
import Rogui.Components.Core (Component (..), DrawM, consoleToExtent, contextCellWidth, emptyComponent)
import Rogui.Components.TextWrap
import Rogui.Components.Types (DrawingContext (..), Extent (..), extentSize)
import Rogui.Graphics (Cell (..))
import Rogui.Graphics.DSL.Instructions (newLine)

-- | Split a text chunk by newline characters, preserving colors.
-- Returns a list of lines, where each line is a list of chunks.
splitChunksByNewlines :: [TextChunk] -> [[TextChunk]]
splitChunksByNewlines chunks = go chunks []
  where
    go [] acc = [reverse acc]
    go ((colour, text) : rest) acc =
      case break (== '\n') text of
        (before, "") ->
          -- No newline in this chunk, continue accumulating
          go rest ((colour, before) : acc)
        (before, _ : after) ->
          -- Found a newline, emit the line and continue with the rest
          let currentLine = reverse ((colour, before) : acc)
              remainingChunks = (colour, after) : rest
           in currentLine : go remainingChunks []

-- | Draw a single line (list of chunks) with wrapping.
-- Returns the number of lines drawn (can be > 1 if wrapping occurs).
drawLine :: Cell -> [TextChunk] -> DrawM n Cell
drawLine _ [] = do
  -- Empty line, just draw a newline
  newLine
  pure 1
drawLine width chunks = do
  -- Filter out empty chunks to avoid spurious spaces
  let nonEmptyChunks = filter (not . null . snd) chunks
  case nonEmptyChunks of
    [] -> do
      -- All chunks were empty, just a blank line
      newLine
      pure 1
    _ -> do
      let totalLength = Cell $ sum $ fmap (length . snd) nonEmptyChunks
      if totalLength > width
        then do
          -- Text is too long, need to wrap
          let (onLine, nextLines) =
                getTextLikeUntil (getCell width) (length . snd) splitChunk nonEmptyChunks
          drawChunksWithSpaces onLine
          newLine
          linesDrawn <- drawLine width nextLines
          pure (1 + linesDrawn)
        else do
          -- Text fits on one line
          drawChunksWithSpaces nonEmptyChunks
          newLine
          pure 1

-- | Draw multiline text without a line limit, handling explicit newlines.
-- Returns the number of lines drawn.
drawMultilineText :: Cell -> [TextChunk] -> DrawM n Cell
drawMultilineText _ [] = pure 0
drawMultilineText width chunks = do
  let textLines = splitChunksByNewlines chunks
  foldM (\count line -> (count +) <$> drawLine width line) 0 textLines

-- | Record an extent with a custom height based on actual rendered lines,
-- while preserving the console's width.
recordExtentWithHeight :: (Ord n) => n -> Cell -> DrawM n ()
recordExtentWithHeight name lineCount = do
  DrawingContext {brush, console, currentExtents} <- get
  let baseExtent = consoleToExtent brush console
      V2 width _ = extentSize baseExtent
      -- Create a modified extent with the actual rendered height
      customExtent = baseExtent {extentSize = V2 width lineCount}
  modify $ \s -> s {currentExtents = M.insert name customExtent currentExtents}

-- | A simple multiline text component that wraps text to fit available width.
--
-- The input is a list of colored text chunks. All chunks are treated as part
-- of the same paragraph and will be wrapped together. Words are kept together
-- when possible, and spaces are added between chunks automatically.
--
-- Explicit line breaks can be added using '\n' characters in the text content.
-- This is useful for paragraphs, item descriptions, or formatted lore text:
--
-- @
-- multilineText MyText
--   [ (colour, "First paragraph here.\n\nSecond paragraph after blank line.")
--   , (colour2, "More text with\nexplicit breaks.")
--   ]
-- @
--
-- For scrolling support, compose this with 'Rogui.Components.Viewport.viewportClipped':
--
-- @
-- viewportClipped MyTextView MyTextContent viewportState $
--   multilineText MyTextContent [(colour, "Your text here...")]
-- @
multilineText :: (Ord n) => n -> [TextChunk] -> Component n
multilineText name chunks =
  let draw' = do
        width <- contextCellWidth
        linesDrawn <- drawMultilineText width chunks
        recordExtentWithHeight name linesDrawn
   in emptyComponent {draw = draw'}
