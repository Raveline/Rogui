{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Rogui.Components.Game.GridTile
  ( gridTile,
    multiLayeredGrid,
  )
where

import Control.Monad.State.Strict
import Data.Foldable (traverse_)
import Rogui.Components (Component (..), Size (..), emptyComponent, layered)
import Rogui.Components.Core (DrawingContext (..), hSize, vSize)
import Rogui.Components.Game.Utils (GlyphInfo (..), MapViewport, computeMapViewport)
import Rogui.Graphics.DSL.Instructions
import Rogui.Graphics.Types (Brush (Brush, tileHeight, tileWidth), Cell, Console (..), (./.=))
import SDL (V2 (..))

cellsInMapViewport :: (V2 Cell, V2 Cell) -> [V2 Cell]
cellsInMapViewport (V2 fromX fromY, V2 toX toY) =
  [V2 x y | x <- [fromX .. toX], y <- [fromY .. toY]]

-- | A classic grid of tiles, typically used to display the
-- environment of a roguelike. All tiles in the viewport
-- will be displayed.
gridTile ::
  -- | A way to obtain an arbitrary tile type from a position.
  (V2 Cell -> t) ->
  -- | How should an arbitrary tile type be rendered
  (t -> GlyphInfo) ->
  -- The map viewport (typically passed through `mutilLayeredGrid`)
  MapViewport ->
  Component n
gridTile getTile getTileDisplay viewport@(topLeft, _) =
  let tilesToPrint = cellsInMapViewport viewport
      renderAt coords = do
        let GlyphInfo {..} = getTileDisplay . getTile $ coords
        setColours colours
        glyphAt (coords - topLeft) glyphId
      draw = traverse_ renderAt tilesToPrint
   in emptyComponent {draw = draw}

-- | A multi layered grid for worlds with cell-based coordinates.
-- It will compute a viewport around a focus, and pass said viewport
-- to all its layers.
multiLayeredGrid ::
  -- | Size of the world
  V2 Cell ->
  -- | Position of the focus point (typically the player)
  V2 Cell ->
  -- | Layers to display with the given viewport
  [MapViewport -> Component n] ->
  Component n
multiLayeredGrid mapDimension focus layers =
  let draw' = do
        Console {width, height} <- gets console
        Brush {tileWidth, tileHeight} <- gets brush
        let viewportHeight = height ./.= tileHeight
            viewportWidth = width ./.= tileWidth
            viewport = computeMapViewport (V2 viewportWidth viewportHeight) mapDimension focus
        draw . vSize (Fixed viewportHeight) . hSize (Fixed viewportWidth) . layered . fmap (\c -> c viewport) $ layers
   in emptyComponent {draw = draw'}