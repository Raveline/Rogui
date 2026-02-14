{-# LANGUAGE FlexibleContexts #-}

module Rogui.Components.Game.EntitiesLayer
  ( entitiesLayer,
    animatedEntitiesLayer,
  )
where

import Control.Monad (when)
import Control.Monad.State.Strict (gets)
import Data.Foldable (traverse_)
import Linear (V2 (..))
import Rogui.Components (Component (..), emptyComponent)
import Rogui.Components.Core (DrawingContext (..))
import Rogui.Components.Game.Utils (GlyphInfo (..), MapViewport, isInViewport)
import Rogui.Graphics (Cell)
import Rogui.Graphics.DSL.Instructions (glyphAt, setColours)

-- | Component to render arbitrary entities on a grid.  Note that we only handle
-- position _inside_ the grid.  This default layer also doesn't handle
-- multi-tile entities natively, if you need these, you're going to either
-- represent these as many entities, or roll your own version of this component.
entitiesLayer ::
  (Foldable f) =>
  -- | A foldable datatype carrying your entity type
  f entity ->
  -- | A function to convert your entity type to GlyphInfo
  (entity -> GlyphInfo) ->
  -- | A function to retrieve the position of your entity type in the world
  (entity -> V2 Cell) ->
  -- | The viewport. Entities that are outside won't be displayed.
  MapViewport ->
  Component m
entitiesLayer entities entityDisplayInfo =
  animatedEntitiesLayer entities (const entityDisplayInfo)

-- | Like `entitiesLayer`, but the rendering function also receives
-- `totalElapsedTime` (in seconds) so it can drive time-based animations
-- (e.g. via `animateCycle`).
animatedEntitiesLayer ::
  (Foldable f) =>
  -- | A foldable datatype carrying your entity type
  f entity ->
  -- | A function to convert your entity type to GlyphInfo, given the elapsed time
  (Double -> entity -> GlyphInfo) ->
  -- | A function to retrieve the position of your entity type in the world
  (entity -> V2 Cell) ->
  -- | The viewport. Entities that are outside won't be displayed.
  MapViewport ->
  Component m
animatedEntitiesLayer entities entityDisplayInfo entityPos viewport@(topLeft, _) =
  let drawSingle elapsed e = do
        let pos = entityPos e
        when (isInViewport viewport pos) $ do
          let GlyphInfo {..} = entityDisplayInfo elapsed e
          setColours colours
          glyphAt (pos - topLeft) glyphId transformations
      draw = do
        elapsed <- gets totalElapsedTime
        traverse_ (drawSingle elapsed) entities
   in emptyComponent {draw = draw}