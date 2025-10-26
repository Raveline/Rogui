{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Rogui.Components.Game.GridTile
  ( gridTile,
    multiLayeredGrid,
  )
where

import Control.Monad.State.Strict
import Data.Foldable (traverse_)
import Rogui.Components (Component (..), Size (..), emptyComponent)
import Rogui.Components.Core (layered)
import Rogui.Components.Game.Utils (GlyphInfo (..), MapViewport, computeMapViewport)
import Rogui.Components.Types (DrawingContext (..), hSize, vSize)
import Rogui.Graphics.DSL.Instructions
import Rogui.Graphics.Types (Brush (Brush, tileHeight, tileWidth), Cell, Console (..), (./.=))
import SDL (V2 (..))

cellsInMapViewport :: (V2 Cell, V2 Cell) -> [V2 Cell]
cellsInMapViewport ((V2 fromX fromY), (V2 toX toY)) =
  [V2 x y | x <- [fromX .. toX], y <- [fromY .. toY]]

gridTile :: (V2 Cell -> t) -> (t -> GlyphInfo) -> MapViewport -> Component n
gridTile getTile getTileDisplay viewport@(topLeft, _) =
  let tilesToPrint = cellsInMapViewport viewport
      renderAt coords = do
        let GlyphInfo {..} = getTileDisplay . getTile $ coords
        setColours colours
        glyphAt (coords - topLeft) glyphId
      draw = traverse_ renderAt tilesToPrint
   in emptyComponent {draw = draw}

multiLayeredGrid :: V2 Cell -> V2 Cell -> [MapViewport -> Component n] -> Component n
multiLayeredGrid mapDimension focus layers =
  let draw' = do
        Console {width, height} <- gets console
        Brush {tileWidth, tileHeight} <- gets brush
        let viewportHeight = height ./.= tileHeight
            viewportWidth = width ./.= tileWidth
            viewport = computeMapViewport (V2 viewportWidth viewportHeight) mapDimension focus
        draw . vSize (Fixed viewportHeight) . hSize (Fixed viewportWidth) . layered . fmap (\c -> c viewport) $ layers
   in emptyComponent {draw = draw'}