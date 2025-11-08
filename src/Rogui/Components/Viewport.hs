{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Utility to add scrolling to a component, through composition.
--
-- @
-- data Names = MyLogMessage
--
-- renderLogs :: ViewportState -> [LogMessage] -> Component Names
-- renderLogs viewportState msgs =
--   vBox [
--     viewport MyLogMessage viewportState $ messageLog msgs
--   ]
-- @
--
-- It is important to note that interactive components offered in Rogui have
-- their own scrolling management: lists and grid do not need to be composed
-- with scrolling.  This was mostly designed to be composed with long text or a
-- message log.
--
-- Note: the first rendered frame might be off there, because we won't have
-- collected the extent data that we need to compute the area available to
-- scroll. Extents are only computed at rendering. This is an acceptable
-- trade-off as it should be invisible to users, unless you're rendering at an
-- incredibly slow FPS.
module Rogui.Components.Viewport
  ( viewport,
    handleViewportEvent,
    scroll,
    ViewportState (..),
  )
where

import Rogui.Application.Event (Event (..), EventHandlerM, KeyDetails (..), KeyDownDetails (..), getExtentSize, modifyState, unhandled)
import Rogui.Components.Core (Component (..), emptyComponent, recordExtent)
import Rogui.Graphics (Cell (..))
import Rogui.Graphics.DSL.Instructions (pencilAt)
import SDL (V2 (V2))
import qualified SDL

-- | This is a simple viewport, which relies heavily on clipping: it moves up
-- the component when scrolling down - the negatives positions will be clipped
-- by SDL.  This is suitable for a small area to scroll, but widely inefficient
-- for huge components, unless they are smart enough to limit their own display.
-- As they need to know their extent to operate, viewport state need to be
-- named.
viewport ::
  (Ord n) =>
  -- | Name for the viewport
  n ->
  -- | Current viewport state
  ViewportState ->
  -- | Component to scroll
  Component n ->
  Component n
viewport name (ViewportState (V2 scrollX scrollY) _) child =
  let draw' = do
        recordExtent name
        pencilAt (V2 (negate scrollX) (negate scrollY))
        draw child
   in emptyComponent {draw = draw'}

-- | A viewport state to handle
data ViewportState = ViewportState
  { scrollOffset :: V2 Cell,
    contentSize :: V2 Cell
  }

-- | Type identifying the type of scrolling requested.
-- Page scrolling is meant to be for vertical scrolling,
-- horizontal pages are not supported.
data ScrollTo = OneDown | OneUp | OneLeft | OneRight | PageDown | PageUp

-- | A helper to update the viewport state depending on the
-- scroll requested. It needs to access the recorded extent.
scroll ::
  (Ord n) =>
  -- | Component name
  n ->
  -- | Scrolling instruction
  ScrollTo ->
  -- | Current viewport state to modify
  ViewportState ->
  EventHandlerM s e n ViewportState
scroll name scrollTo state@ViewportState {scrollOffset, contentSize = (V2 contentX contentY)} = do
  (V2 visibleW visibleH) <- getExtentSize name
  let clampScroll (V2 x y) =
        V2
          (max 0 $ min x (contentX - visibleW))
          (max 0 $ min y (contentY - visibleH))
  pure $ case scrollTo of
    OneDown -> state {scrollOffset = clampScroll $ scrollOffset + V2 0 1}
    OneRight -> state {scrollOffset = clampScroll $ scrollOffset + V2 1 0}
    OneUp -> state {scrollOffset = clampScroll $ scrollOffset - V2 0 1}
    OneLeft -> state {scrollOffset = clampScroll $ scrollOffset - V2 1 0}
    PageDown -> state {scrollOffset = clampScroll $ scrollOffset + V2 0 visibleH}
    PageUp -> state {scrollOffset = clampScroll $ scrollOffset - V2 0 visibleH}

-- | Handle viewport event to manage scrolling.
-- Default supported events are:
--
-- * Arrow keys to scroll by one in all directions;
-- * Page down and page up to scroll by one full page.
handleViewportEvent ::
  (Ord n) =>
  -- | Name of the component, needed to retrieve its extent.
  n ->
  -- | Event to process
  Event e ->
  -- | Current state
  ViewportState ->
  -- | How to update the viewport state in your application state
  (ViewportState -> s -> s) ->
  EventHandlerM s e n ()
handleViewportEvent name event state modifier = do
  case event of
    KeyDown KeyDownDetails {key} -> case keycode key of
      SDL.KeycodePageDown -> scroll name OneDown state >>= modifyState . modifier
      SDL.KeycodePageUp -> scroll name OneUp state >>= modifyState . modifier
      SDL.KeycodeLeft -> scroll name OneLeft state >>= modifyState . modifier
      SDL.KeycodeRight -> scroll name OneRight state >>= modifyState . modifier
      SDL.KeycodeDown -> scroll name OneDown state >>= modifyState . modifier
      SDL.KeycodeUp -> scroll name OneUp state >>= modifyState . modifier
      _ -> unhandled
    _ -> unhandled