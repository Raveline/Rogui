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
    viewportClipped,
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

-- | This is an optimized viewport where the child component receives the scroll
-- offset as a parameter, allowing it to efficiently render only the visible
-- portion plus the scrolled area. This is suitable for very large components
-- like message logs that can optimize their rendering.
--
-- For simpler use cases where the content is not huge, see 'viewportClipped'.
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

-- | A simpler viewport that relies on SDL clipping.
--
-- This viewport simply offsets the child component and lets SDL clip anything
-- that falls outside the viewport bounds. This is much simpler to use than
-- 'viewport' because the child component doesn't need to be aware of scrolling.
--
-- This is suitable for smaller content areas (like text that isn't extremely
-- long) where rendering the entire content is acceptable. The rendering is still
-- efficient because SDL will clip anything outside the visible area.
--
-- For this to work, you will need to ensure that the child has a dedicated
-- way of recording its extent (not via `recordExtent` or `withRecordedExtent`,
-- which only takes the console space available). You need a logic inside the
-- component itself. `multiLineText` is a good example of this.
--
-- @
-- data Names = TextViewport TextContent
--
-- renderText :: ViewportState -> Component Names
-- renderText viewportState =
--   viewportClipped TextViewport viewportState $
--     multilineText TextContent
--       [ (Colours (Just white) (Just black), "Long text here...")
--       , (Colours (Just red) Nothing, "More text...")
--       ]
--
-- handleEvents state event =
--   handleViewportEvent TextViewport TextContent viewportState updater state event
-- @
viewportClipped ::
  (Ord n) =>
  -- | Name for the viewport (to query its visible extent)
  n ->
  -- | Current viewport state
  ViewportState ->
  -- | Child component to scroll
  Component n ->
  Component n
viewportClipped viewportName (ViewportState (V2 scrollX scrollY) _) child =
  let draw' = do
        recordExtent viewportName
        pencilAt (V2 (negate scrollX) (negate scrollY))
        draw child
   in emptyComponent {draw = draw'}

-- | A viewport state to handle scrolling.
--
-- The contentSize field determines the maximum scroll bounds. If either
-- component is 0, scrolling in that direction is unlimited (useful when
-- the actual content size is unknown or when using viewportClipped with
-- content that will be clipped by SDL anyway).
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
          -- If content is 0, allow unlimited scrolling
          (if contentX == 0 then max 0 x else max 0 $ min x (contentX - visibleW))
          (if contentY == 0 then max 0 y else max 0 $ min y (contentY - visibleH))
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
  -- | Name of the viewport (to query visible extent for clamping)
  n ->
  -- | Name of the child content (to query content extent for updating state)
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
  -- | Name of the viewport (to query visible extent for clamping)
  n ->
  -- | Name of the child content (to query content extent for updating state)
  n ->
  -- | Current state
  ViewportState ->
  -- | How to update the viewport state in your application state
  (ViewportState -> s -> s) ->
  EventHandler m s e n
handleViewportEvent' keyMap viewportName childName state modifier =
  let toEvent e _ _ = redraw $ do
        updatedState <-
          if contentSize state == V2 0 0
            then do
              contentSize' <- getExtentSize childName
              pure $ state {contentSize = contentSize'}
            else
              pure state
        scroll viewportName e updatedState >>= modifyState . modifier
   in keyPressHandler (second toEvent <$> keyMap)
