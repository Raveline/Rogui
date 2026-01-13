module Rogui.Components.Game.GridOverlay
  ( gridOverlay,
  )
where

import Data.Foldable (traverse_)
import Linear (V2)
import Rogui.Components.Core (DrawM)
import Rogui.Components.Game.Utils (MapViewport, cellsInMapViewport)
import Rogui.Components.Types (Component (..), emptyComponent)
import Rogui.Graphics

-- | A grid containing only overlay instructions, typically
-- used for lighting, FOV, and all type of colour / transparency
-- changes to apply over the viewport.
gridOverlay ::
  -- | Given a world position, what colour (if any) with alpha should we overlay over ?
  (V2 Cell -> DrawM n (Maybe RGBA)) ->
  -- | SDL BlendMode to apply on overlay. If you specify nothing, we will use the basic Alpha Blend.
  Maybe BlendMode ->
  -- | Viewport in use. This is typically retrieved through `Rogui.Components.Game.GridTile.multiLayeredGrid`.
  MapViewport ->
  Component n
gridOverlay getOverlay mode viewport@(topLeft, _) =
  let draw' = traverse_ drawCell (cellsInMapViewport viewport)
      drawCell worldPos = do
        overlay <- getOverlay worldPos
        maybe (pure ()) (\o -> overlayAt (worldPos - topLeft) o mode) overlay
   in emptyComponent {draw = draw'}