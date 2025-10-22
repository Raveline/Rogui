{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Rogui.Components.Viewport
  ( viewport,
    handleViewportEvent,
    autoScrollToSelection,
    ViewportState (..),
  )
where

import Rogui.Application.Event (Event (..), EventHandlingM, KeyDownDetails (..), modifyState)
import Rogui.Components.Types (Component (..), emptyComponent)
import Rogui.Graphics (Cell (..))
import Rogui.Graphics.DSL.Instructions (pencilAt)
import SDL (V2 (V2))
import qualified SDL

-- | This is a simple viewport, which relies heavily on clipping:
-- it moves up the component when scrolling down - the negatives
-- positions will be clipped by SDL.
-- This is suitable for a small area to scroll, but widely
-- inefficient for huge components.
viewport :: V2 Cell -> Component n -> Component n
viewport (V2 scrollX scrollY) child =
  let draw' = do
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

handleViewportEvent :: V2 Cell -> Event e -> ViewportState -> (ViewportState -> s -> s) -> EventHandlingM s e ()
handleViewportEvent (V2 visibleW visibleH) event state@ViewportState {scrollOffset, contentSize = (V2 contentX contentY)} modifier =
  case event of
    KeyDown KeyDownDetails {key} -> case SDL.keysymKeycode key of
      SDL.KeycodePageDown -> modifyState $ modifier $ state {scrollOffset = clampScroll $ scrollOffset + V2 0 visibleH}
      SDL.KeycodePageUp -> modifyState $ modifier $ state {scrollOffset = clampScroll $ scrollOffset - V2 0 visibleH}
      SDL.KeycodeDown -> modifyState $ modifier $ state {scrollOffset = clampScroll $ scrollOffset + V2 0 1}
      SDL.KeycodeUp -> modifyState $ modifier $ state {scrollOffset = clampScroll $ scrollOffset - V2 0 1}
      _ -> pure ()
    _ -> pure ()
  where
    clampScroll (V2 x y) =
      V2
        (max 0 $ min x (contentX - visibleW))
        (max 0 $ min y (contentY - visibleH))