{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Rogui.Components.Game.GridTile
  ( gridTile,
    multiLayeredGrid,
    mouseEventToWorldPos,
  )
where

import Control.Monad.State.Strict
import Data.Foldable (traverse_)
import qualified Data.Map.Strict as M
import Rogui.Application
import Rogui.Components (Component (..), Size (..), emptyComponent, layered)
import Rogui.Components.Core (DrawingContext (..), Extent (..), TileSize, hSize, vSize)
import Rogui.Components.Game.Utils (GlyphInfo (..), MapViewport, cellsInMapViewport, computeMapViewport)
import Rogui.Graphics (Pixel, v2PixelInTiles)
import Rogui.Graphics.DSL.Instructions
import Rogui.Graphics.Types (Brush (Brush, tileHeight, tileWidth), Cell, Console (..), (./.=))
import SDL (V2 (..))

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
        glyphAt (coords - topLeft) glyphId transformations
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

-- | Compute the corresponding world coordinates (if mouse coord hit the
-- extent).  By default, grids do not record their extent, so if you need to get
-- this, you'll want to wrap your grid (or `multiLayeredGrid`) in a
-- `Rogui.Components.Core.withRecordedExtent` component.
--
-- This function is typically used to support mouse interaction on the grid;
-- for instance to display information on the tile under the cursor,
-- or to support picking a target entity with the mouse.
mouseEventToWorldPos ::
  (Ord n, Monad m) =>
  -- \| The extent name for the grid
  n ->
  -- | The tilesize used for rendering the grid
  TileSize ->
  -- | The max width and height of the map
  V2 Cell ->
  -- | The focus point of the grid (usually, player position)
  V2 Cell ->
  -- | A mouse position in pixel
  V2 Pixel ->
  EventHandlerM m s e n (Maybe (V2 Cell))
mouseEventToWorldPos n tilesize mapDimension focus mousePos = do
  result <- liftEH $ gets (M.lookup n . knownExtents)
  let withExtent Extent {..} =
        let (viewportStart, _) = computeMapViewport extentSize mapDimension focus
         in v2PixelInTiles (mousePos - position extentConsole) tilesize + viewportStart
  pure $ result >>= Just . withExtent