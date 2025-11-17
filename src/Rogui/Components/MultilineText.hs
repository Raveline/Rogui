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
  )
where

import Control.Monad.State.Strict (get, modify)
import qualified Data.Map as M
import Rogui.Components.Core (Component (..), DrawM, consoleToExtent, contextCellWidth, emptyComponent)
import Rogui.Components.TextWrap
import Rogui.Components.Types (DrawingContext (..), Extent (..), extentSize)
import Rogui.Graphics (Cell (..))
import Rogui.Graphics.DSL.Instructions (newLine)
import SDL (V2 (..))

-- | Draw multiline text without a line limit.
-- Returns the number of lines drawn.
drawMultilineText :: Cell -> [TextChunk] -> DrawM n Cell
drawMultilineText _ [] = pure 0
drawMultilineText width chunks = do
  let totalLength = Cell $ sum $ fmap (length . snd) chunks
  if totalLength > width
    then do
      -- Text is too long, need to wrap
      let (onLine, nextLines) =
            getTextLikeUntil (getCell width) (length . snd) splitChunk chunks
      drawChunksWithSpaces onLine
      newLine
      linesDrawn <- drawMultilineText width nextLines
      pure (1 + linesDrawn)
    else do
      -- Text fits on one line
      drawChunksWithSpaces chunks
      newLine
      pure 1

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
