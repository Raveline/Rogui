module Rogui.Components.Game.Utils
  ( GlyphInfo (..),
    MapViewport,
    computeMapViewport,
    isInViewport,
  )
where

import Rogui.Graphics (Colours, Transformation)
import Rogui.Graphics.Types (Cell)
import SDL (V2 (..))

-- | The canonical representation of a single-cell glyph.
-- Expects an arbitrary id number, a set of colours
-- and transformations if any
data GlyphInfo = GlyphInfo
  { -- | The id of the glyph in your tileset / charset. For ASCII characters, if they are properly positionned in your tileset, you can use `ord` from Data.Char to retrieve their id.
    glyphId :: Int,
    -- | The colours for this glyph
    colours :: Colours,
    -- | Potential transformations on this glyph
    transformations :: [Transformation]
  }

-- | A map viewport is a pair of top-left, bottom-right bounds.
type MapViewport = (V2 Cell, V2 Cell)

-- | Compute all the tiles to display from a given center
-- point. If center is too close to the border of the actual
-- tilemap to display, we'll adjust accordingly.
--
-- Note that is assumed that your coordinate system starts at (0,0),
-- the basic game components provided by rogui do not support negative
-- coordinates.
computeMapViewport ::
  -- | Width and height of the viewport
  V2 Cell ->
  -- | Max width and height of the map to display
  V2 Cell ->
  -- | Focus point on which to center (typically the player position)
  V2 Cell ->
  MapViewport
computeMapViewport (V2 viewportWidth viewportHeight) (V2 mapWidth mapHeight) (V2 focusX focusY) =
  let idealFromX = focusX - (viewportWidth `div` 2)
      idealFromY = focusY - (viewportHeight `div` 2)
      fromX = max 0 (min idealFromX (mapWidth - viewportWidth))
      toX = min mapWidth (fromX + viewportWidth)
      fromY = max 0 (min idealFromY (mapHeight - viewportHeight))
      toY = min mapHeight (fromY + viewportHeight)
   in (V2 fromX fromY, V2 toX toY)

-- | Check if a cell is in the given viewport.
isInViewport :: MapViewport -> V2 Cell -> Bool
isInViewport (V2 fromX fromY, V2 toX toY) (V2 x y) =
  x >= fromX && x <= toX && y >= fromY && y <= toY