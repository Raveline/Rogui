{-# LANGUAGE FlexibleContexts #-}

-- | Shared utilities for text wrapping and rendering colored text chunks.
--
-- This module provides reusable functions for components that need to wrap
-- multi-colored text to fit within a given width.
module Rogui.Components.TextWrap
  ( TextChunk,
    splitChunk,
    getTextLikeUntil,
    drawChunk,
    drawChunksWithSpaces,
  )
where

import Data.Foldable
import Data.List (intersperse)
import qualified Data.Sequence as Seq
import Rogui.Components.Core (DrawM)
import Rogui.Graphics (Colours, TextAlign (..), setColours, str)

-- | A chunk of text with associated colors.
type TextChunk = (Colours, String)

-- | Split a text chunk at a given width, trying to break on word boundaries.
-- Returns (Just fitted_chunk, remaining_chunk) if we can fit something,
-- or (Nothing, original_chunk) if we can't fit anything.
--
-- If a single word is longer than the available width, nothing fits and
-- the entire chunk is returned as remaining.
splitChunk :: Int -> TextChunk -> (Maybe TextChunk, TextChunk)
splitChunk n (rgb, content) =
  let splitByWord = words content
      (fittingWords, nonFittingWord) =
        getTextLikeUntil n (\w -> length w + 1) (\_ t -> (Nothing, t)) splitByWord
   in if not . null $ fittingWords
        then (Just (rgb, unwords fittingWords), (rgb, unwords nonFittingWord))
        else (Nothing, (rgb, content))

-- | Generic helper to split a list of items until they exceed a width limit.
--
-- Takes:
-- * width: Maximum width allowed
-- * getLength: Function to get the length/size of an item
-- * breaker: Function to break an item if it's too big
-- * ts: List of items to process
--
-- Returns (items_that_fit, items_that_dont_fit).
--
-- This function accumulates items until adding the next item would exceed
-- the width. When an item is too big, it calls the breaker function to
-- try to split it. The breaker returns (Maybe fitted_part, remaining_part).
getTextLikeUntil ::
  Int ->
  (t -> Int) ->
  (Int -> t -> (Maybe t, t)) ->
  [t] ->
  ([t], [t])
getTextLikeUntil width getLength breaker ts =
  let folder (size, collected, left) item
        | (getLength item + size) > width =
            -- Item is too big. Let's try to break it
            case breaker (width - size) item of
              (Just enough, tooBig) -> (width, collected Seq.|> enough, left Seq.|> tooBig)
              (Nothing, tooBig) -> (width, collected, left Seq.|> tooBig)
        | otherwise =
            (size + getLength item, collected Seq.|> item, left)
      getResult (_, taken, left) = (toList taken, toList left)
   in getResult . foldl' folder (0, Seq.empty, Seq.empty) $ ts

-- | Draw a single text chunk with its colors.
drawChunk :: TextChunk -> DrawM n ()
drawChunk (chunkColour, chunkTxt) = do
  setColours chunkColour
  str TLeft chunkTxt

-- | Draw a list of text chunks with spaces between them.
-- Spaces are automatically added between chunks.
drawChunksWithSpaces :: [TextChunk] -> DrawM n ()
drawChunksWithSpaces chunks =
  traverse_ drawChunkOrSpace (intersperse (Left " ") (Right <$> chunks))
  where
    drawChunkOrSpace (Left space) = str TLeft space
    drawChunkOrSpace (Right chunk) = drawChunk chunk
