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
    defaultViewportKeys,
    handleViewportEvent,
    handleViewportEvent',
    scroll,
    ViewportState (..),
    ViewportAction (..),
  )
where

import Data.Bifunctor
import Rogui.Application.Event
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
--
-- The child component receives the current scroll offset as a parameter, allowing
-- it to efficiently render only the visible portion plus the scrolled area.
viewport ::
  (Ord n) =>
  -- | Name for the viewport
  n ->
  -- | Current viewport state
  ViewportState ->
  -- | Function that takes scroll offset and returns component to scroll
  (V2 Cell -> Component n) ->
  Component n
viewport name (ViewportState scrollOffset@(V2 scrollX scrollY) _) childFn =
  let draw' = do
        recordExtent name
        pencilAt (V2 (negate scrollX) (negate scrollY))
        draw (childFn scrollOffset)
   in emptyComponent {draw = draw'}

-- | A viewport state to handle
data ViewportState = ViewportState
  { scrollOffset :: V2 Cell,
    contentSize :: V2 Cell
  }

-- | Type identifying the type of scrolling requested.
-- Page scrolling is meant to be for vertical scrolling,
-- horizontal pages are not supported.
data ViewportAction = OneDown | OneUp | OneLeft | OneRight | PageDown | PageUp

defaultViewportKeys :: [(KeyDetailsMatch, ViewportAction)]
defaultViewportKeys =
  [ (isSC' SDL.ScancodeDown, OneDown),
    (isSC' SDL.ScancodeUp, OneUp),
    (isSC' SDL.ScancodeLeft, OneLeft),
    (isSC' SDL.ScancodeRight, OneRight),
    (isSC' SDL.ScancodePageDown, PageDown),
    (isSC' SDL.ScancodePageUp, PageUp)
  ]

-- | A helper to update the viewport state depending on the
-- scroll requested. It needs to access the recorded extent.
scroll ::
  (Monad m, Ord n) =>
  -- | Component name
  n ->
  -- | Scrolling instruction
  ViewportAction ->
  -- | Current viewport state to modify
  ViewportState ->
  EventHandlerM m s e n ViewportState
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
-- Default supported keys are:
--
-- * Arrow keys to scroll by one in all directions;
-- * Page down and page up to scroll by one full page.
handleViewportEvent ::
  (Monad m, Ord n) =>
  -- | Name of the component, needed to retrieve its extent.
  n ->
  -- | Current state
  ViewportState ->
  -- | How to update the viewport state in your application state
  (ViewportState -> s -> s) ->
  EventHandler m s e n
handleViewportEvent = handleViewportEvent' defaultViewportKeys

handleViewportEvent' ::
  (Monad m, Ord n) =>
  -- | Mapping of key to actions
  [(KeyDetailsMatch, ViewportAction)] ->
  -- | Name of the component, needed to retrieve its extent.
  n ->
  -- | Current state
  ViewportState ->
  -- | How to update the viewport state in your application state
  (ViewportState -> s -> s) ->
  EventHandler m s e n
handleViewportEvent' keyMap name state modifier =
  let toEvent e _ _ = scroll name e state >>= modifyState . modifier
   in keyPressHandler (second toEvent <$> keyMap)
