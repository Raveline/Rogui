{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Rogui.Components.Viewport
  ( viewport,
    handleViewportEvent,
    autoScrollToSelection,
    ViewportState (..),
  )
where

import Rogui.Application.Event (Event (..), EventHandlingM, KeyDownDetails (..), getExtentSize, modifyState)
import Rogui.Components.Types (Component (..), emptyComponent, recordExtent)
import Rogui.Graphics (Cell (..))
import Rogui.Graphics.DSL.Instructions (pencilAt)
import SDL (V2 (V2))
import qualified SDL

-- | This is a simple viewport, which relies heavily on clipping:
-- it moves up the component when scrolling down - the negatives
-- positions will be clipped by SDL.
-- This is suitable for a small area to scroll, but widely
-- inefficient for huge components, unless they are smart
-- enough to limit their own display.
viewport :: (Ord n) => n -> V2 Cell -> Component n -> Component n
viewport name (V2 scrollX scrollY) child =
  let draw' = do
        recordExtent name
        pencilAt (V2 (negate scrollX) (negate scrollY))
        draw child
   in emptyComponent {draw = draw'}

data ViewportState = ViewportState
  { scrollOffset :: V2 Cell,
    contentSize :: V2 Cell
  }

autoScrollToSelection :: Cell -> Int -> ViewportState -> ViewportState
autoScrollToSelection visibleHeight selection state@ViewportState {scrollOffset = V2 _ y}
  | Cell selection < y = state {scrollOffset = V2 0 (Cell selection)}
  | Cell selection >= y + visibleHeight =
      state {scrollOffset = V2 0 (Cell selection - visibleHeight + 1)}
  | otherwise = state

-- | Note: the first rendered frame might be off there, because we won't have
-- collected the extent data. Extents are only computed at rendering. This is
-- an acceptable trade-off as it should be invisible to users, unless you're
-- rendering at an incredibly slow FPS.
handleViewportEvent :: (Ord n) => n -> Event e -> ViewportState -> (ViewportState -> s -> s) -> EventHandlingM s e n ()
handleViewportEvent name event state@ViewportState {scrollOffset, contentSize = (V2 contentX contentY)} modifier = do
  (V2 visibleW visibleH) <- getExtentSize name
  let clampScroll (V2 x y) =
        V2
          (max 0 $ min x (contentX - visibleW))
          (max 0 $ min y (contentY - visibleH))
  case event of
    KeyDown KeyDownDetails {key} -> case SDL.keysymKeycode key of
      SDL.KeycodePageDown -> modifyState $ modifier $ state {scrollOffset = clampScroll $ scrollOffset + V2 0 visibleH}
      SDL.KeycodePageUp -> modifyState $ modifier $ state {scrollOffset = clampScroll $ scrollOffset - V2 0 visibleH}
      SDL.KeycodeDown -> modifyState $ modifier $ state {scrollOffset = clampScroll $ scrollOffset + V2 0 1}
      SDL.KeycodeUp -> modifyState $ modifier $ state {scrollOffset = clampScroll $ scrollOffset - V2 0 1}
      _ -> pure ()
    _ -> pure ()