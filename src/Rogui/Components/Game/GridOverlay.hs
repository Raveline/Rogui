module Rogui.Components.Game.GridOverlay
  ( gridOverlay,
  )
where

import Data.Foldable (traverse_)
import Rogui.Components.Core (DrawM)
import Rogui.Components.Game.Utils (MapViewport, cellsInMapViewport)
import Rogui.Components.Types (Component (..), emptyComponent)
import Rogui.Graphics.DSL.Instructions (overlayAt)
import Rogui.Graphics.Primitives (RGBA)
import Rogui.Graphics.Types (Cell)
import SDL (V2)

-- Add this as another layer in multiLayeredGrid
gridOverlay ::
  (V2 Cell -> DrawM n (Maybe RGBA)) -> -- Function: world pos -> overlay color
  MapViewport ->
  Component n
gridOverlay getOverlay viewport@(topLeft, _) =
  let draw' = traverse_ drawCell (cellsInMapViewport viewport)
      drawCell worldPos = do
        overlay <- getOverlay worldPos
        maybe (pure ()) (overlayAt (worldPos - topLeft)) overlay
   in emptyComponent {draw = draw'}