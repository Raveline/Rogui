{-# LANGUAGE FlexibleContexts #-}

module Rogui.Components.Game.GridTile
  ( gridTile,
    multiLayeredGrid,
  )
where

import Data.Foldable (traverse_)
import Rogui.Components (Component (..), Size (..), emptyComponent)
import Rogui.Components.Core (layered)
import Rogui.Components.Game.Utils (GlyphInfo (..), MapViewport)
import Rogui.Components.Types (hSize, vSize)
import Rogui.Graphics (Brush)
import Rogui.Graphics.DSL.Instructions
import Rogui.Graphics.Types (Cell)
import SDL (V2 (..))

cellsInMapViewport :: (V2 Cell, V2 Cell) -> [V2 Cell]
cellsInMapViewport ((V2 fromX fromY), (V2 toX toY)) =
  [V2 x y | x <- [fromX .. toX], y <- [fromY .. toY]]

gridTile :: Brush -> V2 Cell -> (V2 Cell -> t) -> (t -> GlyphInfo) -> MapViewport -> Component n
gridTile tileBrush (V2 viewportWidth viewportHeight) getTile getTileDisplay viewport@(topLeft, _) =
  let tilesToPrint = cellsInMapViewport viewport
      renderAt coords = do
        let GlyphInfo {..} = getTileDisplay . getTile $ coords
        setColours colours
        glyphAt (coords - topLeft) glyphId
      draw _ = do
        withBrush tileBrush
        traverse_ renderAt tilesToPrint
   in emptyComponent {draw = draw, horizontalSize = Fixed viewportWidth, verticalSize = Fixed viewportHeight}

multiLayeredGrid :: V2 Cell -> MapViewport -> [MapViewport -> Component n] -> Component n
multiLayeredGrid (V2 viewportWidth viewportHeight) viewport =
  vSize (Fixed viewportHeight) . hSize (Fixed viewportWidth) . layered . fmap (\c -> c viewport)