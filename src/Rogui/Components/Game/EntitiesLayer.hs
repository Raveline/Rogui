{-# LANGUAGE FlexibleContexts #-}

module Rogui.Components.Game.EntitiesLayer
  ( entitiesLayer,
  )
where

import Control.Monad (when)
import Data.Foldable (traverse_)
import Rogui.Components (Component (..), emptyComponent)
import Rogui.Components.Game.Utils (GlyphInfo (..), MapViewport)
import Rogui.Graphics (Cell)
import Rogui.Graphics.DSL.Instructions (glyphAt, setColours)
import SDL (V2 (..))

isInViewport :: MapViewport -> V2 Cell -> Bool
isInViewport (V2 fromX fromY, V2 toX toY) (V2 x y) =
  x >= fromX && x <= toX && y >= fromY && y <= toY

entitiesLayer :: (Foldable f) => f entity -> (entity -> GlyphInfo) -> (entity -> V2 Cell) -> MapViewport -> Component m
entitiesLayer entities entityDisplayInfo entityPos viewport@(topLeft, _) =
  let drawSingle e = do
        let pos = entityPos e
        when (isInViewport viewport $ entityPos e) $ do
          let GlyphInfo {..} = entityDisplayInfo e
          setColours colours
          glyphAt (pos - topLeft) glyphId
      draw = do
        traverse_ drawSingle entities
   in emptyComponent {draw = draw}