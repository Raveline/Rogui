{-# LANGUAGE FlexibleContexts #-}

module Rogui.Components.MessageLog
  ( messageLog,
    handleMessageLogEvent,
    calculateMessageLogHeight,
    LogChunk,
    LogMessage,
  )
where

import Control.Monad (void)
import Data.Foldable
import Rogui.Application.Event
import Rogui.Components.Core (Component (..), DrawM, contextCellHeight, contextCellWidth, emptyComponent)
import Rogui.Components.TextWrap
import Rogui.Components.Viewport (ViewportAction, ViewportState (..), defaultViewportKeys, handleViewportEvent', viewport)
import Rogui.Graphics (Cell (..))
import Rogui.Graphics.DSL.Instructions (newLine)
import SDL (V2 (..))

type LogChunk = TextChunk

type LogMessage = [LogChunk]

drawMessageLog :: Cell -> LogMessage -> Cell -> DrawM n Cell
drawMessageLog _ _ 0 = pure 0
drawMessageLog width msg remainingLines = do
  let msgLength = Cell $ sum $ fmap (length . snd) msg
  if msgLength > width
    then do
      let (onLine, nextLines) = getTextLikeUntil (getCell width) (length . snd) splitChunk msg
      drawChunksWithSpaces onLine
      newLine
      drawMessageLog width nextLines (remainingLines - 1)
    else do
      drawChunksWithSpaces msg
      newLine
      pure (remainingLines - 1)

truncateWordBy :: Int -> String -> String
truncateWordBy beyond w
  | length w > beyond = take (beyond - 3) w <> "..."
  | otherwise = w

truncateWordsOverWidth :: Int -> LogMessage -> LogMessage
truncateWordsOverWidth beyond msg =
  let over :: LogChunk -> LogChunk
      over = fmap (unwords . fmap (truncateWordBy beyond) . words)
   in fmap over msg

-- | Count how many lines a single message will take up when rendered with the given width.
-- Accounts for text wrapping.
countMessageLines :: Cell -> LogMessage -> Cell
countMessageLines _ [] = 0
countMessageLines width msg =
  let msgLength = Cell $ sum $ fmap (length . snd) msg
   in if msgLength > width
        then
          let (_, nextLines) = getTextLikeUntil (getCell width) (length . snd) splitChunk msg
           in 1 + countMessageLines width nextLines
        else 1

-- | Calculate the total number of lines needed to render a list of messages
-- with the given width, accounting for text wrapping.
calculateMessageLogHeight :: (Functor f, Foldable f) => Cell -> f LogMessage -> Cell
calculateMessageLogHeight width msgs =
  let truncated = truncateWordsOverWidth (getCell width) <$> msgs
   in sum $ fmap (countMessageLines width) truncated

drawMessageLogs :: (Functor f, Foldable f) => V2 Cell -> f LogMessage -> DrawM n ()
drawMessageLogs (V2 _ scrollY) msgs = do
  maxWidth <- contextCellWidth
  visibleLines <- contextCellHeight
  -- When used with viewport, we need to render scrollY + visibleLines
  -- to account for the portion that will be clipped by viewport's negative positioning
  let totalLinesToRender = scrollY + visibleLines
      truncated = fmap (truncateWordsOverWidth $ getCell maxWidth) msgs
  void $ foldrM (drawMessageLog maxWidth) totalLinesToRender truncated

-- | A message log component with built-in scrolling support.
--
-- This component can internally use a viewport for scrolling. The viewport state
-- is managed by the application and should be updated via 'handleMessageLogEvent'.
-- The content size in the ViewportState is ignored - it's calculated dynamically
-- based on text wrapping.
--
-- Logs are displayed in the _reverse_ order. In a typical roguelike, the log
-- would probably be a Sequence or a Vector or any type where getting the last
-- elements should not have a high complexity.
--
-- Long lines will be wrapped, words that exceed the width will be truncated.
-- This implementation cuts on whitespaces when wrapping, so any intentional
-- double whitespace will get removed.
--
-- Usage:
-- @
-- data Names = LogView
-- data State = State { logViewport :: ViewportState, messages :: [LogMessage] }
--
-- render state = messageLog LogView (logViewport state) (messages state)
--
-- eventHandler state event =
--   handleMessageLogEvent LogView (Just $ logViewport state) (messages state) event
--     (\\newViewport s -> s { logViewport = newViewport })
-- @
messageLog :: (Ord n, Foldable f, Functor f) => n -> Maybe ViewportState -> f LogMessage -> Component n
messageLog name viewportState msgs =
  case viewportState of
    Just vs -> viewport name vs $ \scrollOffset -> emptyComponent {draw = drawMessageLogs scrollOffset msgs}
    Nothing -> emptyComponent {draw = drawMessageLogs (V2 0 0) msgs}

-- | Handle message log events for scrolling.
--
-- This calculates the content size dynamically based on the messages and visible width,
-- then delegates to the viewport's event handler for actual scrolling logic.
--
-- NB: computing the actual size of the full logs might end up being a bit slow.
-- If this becomes a problem, you'll need to come up with your own solution here.
-- (One of them being to cache computations and only recompute when a new log
-- gets added; or if you know you'll always have enough size to display logs
-- it becomes as simple as computing the length).
--
-- Default supported events are the one from `Rogui.Components.Viewport.handleViewportEvent`.
-- You can use different mappings with `handleMessageLogEvent'`.
handleMessageLogEvent ::
  (Monad m, Ord n, Foldable f, Functor f) =>
  -- | Name of the component, needed to retrieve its extent
  n ->
  -- | Current messages to calculate content height
  f LogMessage ->
  -- | Current viewport state
  ViewportState ->
  -- | How to update the viewport state in your application state
  (ViewportState -> s -> s) ->
  EventHandler m s e n
handleMessageLogEvent = do
  handleMessageLogEvent' defaultViewportKeys

handleMessageLogEvent' ::
  (Monad m, Ord n, Foldable f, Functor f) =>
  [(KeyMatch, ViewportAction)] ->
  -- | Name of the component, needed to retrieve its extent
  n ->
  -- | Current messages to calculate content height
  f LogMessage ->
  -- | Current viewport state
  ViewportState ->
  -- | How to update the viewport state in your application state
  (ViewportState -> s -> s) ->
  EventHandler m s e n
handleMessageLogEvent' keyMap name msgs state modifier s e = do
  (V2 visibleW _) <- getExtentSize name
  let contentHeight = calculateMessageLogHeight visibleW msgs
      updatedState = state {contentSize = V2 0 contentHeight}
  -- MessageLog uses optimized viewport, so viewport and child have same name
  handleViewportEvent' keyMap name name updatedState modifier s e