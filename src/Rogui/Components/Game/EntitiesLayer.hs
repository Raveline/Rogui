{-# LANGUAGE FlexibleContexts #-}

module Rogui.Components.Game.EntitiesLayer
  ( entitiesLayer,
  )
where

import Control.Monad (when)
import Data.Foldable (traverse_)
import Rogui.Components (Component (..), emptyComponent)
import Rogui.Components.Game.Utils (GlyphInfo (..), MapViewport, isInViewport)
import Rogui.Graphics (Cell)
import Rogui.Graphics.DSL.Instructions (glyphAt, setColours)
import SDL (V2 (..))

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
entitiesLayer entities entityDisplayInfo entityPos viewport@(topLeft, _) =
  let drawSingle e = do
        let pos = entityPos e
        when (isInViewport viewport $ entityPos e) $ do
          let GlyphInfo {..} = entityDisplayInfo e
          setColours colours
          glyphAt (pos - topLeft) glyphId transformations
      draw = do
        traverse_ drawSingle entities
   in emptyComponent {draw = draw}