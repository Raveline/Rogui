{-# LANGUAGE FlexibleContexts #-}

module Rogui.Components.GridTile
  ( gridTile,
    GlyphInfo (..),
  )
where

import Data.Foldable (traverse_)
import Rogui.Components (Component (..), Size (..), emptyComponent)
import Rogui.Graphics (Brush)
import Rogui.Graphics.DSL.Instructions
import SDL (V2 (..))

data GlyphInfo = GlyphInfo
  { glyphId :: Int,
    colours :: Colours
  }

-- | Compute all the tiles to display from a given center
-- point. If center is too close to the border of the actual
-- tilemap to display, we'll display more.
-- The first item returned is the top-left most tile, which
-- can be used to get local coordinates easily.
computeViewport :: V2 Int -> V2 Int -> V2 Int -> (V2 Int, [V2 Int])
computeViewport (V2 viewportWidth viewportHeight) (V2 mapWidth mapHeight) (V2 focusX focusY) =
  let idealFromX = focusX - (viewportWidth `div` 2)
      idealFromY = focusY - (viewportHeight `div` 2)
      fromX = max 0 (min idealFromX (mapWidth - viewportWidth))
      toX = min mapWidth (fromX + viewportWidth)
      fromY = max 0 (min idealFromY (mapHeight - viewportHeight))
      toY = min mapHeight (fromY + viewportHeight)
      allCoords = [V2 x y | x <- [fromX .. toX], y <- [fromY .. toY]]
   in ((V2 fromX fromY), allCoords)

gridTile :: Brush -> V2 Int -> V2 Int -> (V2 Int -> t) -> (t -> GlyphInfo) -> V2 Int -> Component n
gridTile tileBrush viewportSize@(V2 viewportWidth viewportHeight) mapSize getTile getTileDisplay focusOn =
  let (from, tilesToPrint) = computeViewport viewportSize mapSize focusOn
      renderAt coords = do
        let GlyphInfo {..} = getTileDisplay . getTile $ coords
        setColours colours
        glyphAt (coords - from) glyphId
      draw _ = do
        withBrush tileBrush
        traverse_ renderAt tilesToPrint
   in emptyComponent {draw = draw, hSize = Fixed viewportWidth, vSize = Fixed viewportHeight}