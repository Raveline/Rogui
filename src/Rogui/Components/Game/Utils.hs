module Rogui.Components.Game.Utils
  ( GlyphInfo (..),
    MapViewport,
    computeMapViewport,
  )
where

import Rogui.Graphics (Colours)
import Rogui.Graphics.Types (Cell)
import SDL (V2 (..))

data GlyphInfo = GlyphInfo
  { glyphId :: Int,
    colours :: Colours
  }

type MapViewport = (V2 Cell, V2 Cell)

-- | Compute all the tiles to display from a given center
-- point. If center is too close to the border of the actual
-- tilemap to display, we'll display more.
-- This returns a pair with the top left and bottom right
-- coords of the viewport.
computeMapViewport :: V2 Cell -> V2 Cell -> V2 Cell -> MapViewport
computeMapViewport (V2 viewportWidth viewportHeight) (V2 mapWidth mapHeight) (V2 focusX focusY) =
  let idealFromX = focusX - (viewportWidth `div` 2)
      idealFromY = focusY - (viewportHeight `div` 2)
      fromX = max 0 (min idealFromX (mapWidth - viewportWidth))
      toX = min mapWidth (fromX + viewportWidth)
      fromY = max 0 (min idealFromY (mapHeight - viewportHeight))
      toY = min mapHeight (fromY + viewportHeight)
   in (V2 fromX fromY, V2 toX toY)
