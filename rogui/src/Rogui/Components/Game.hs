-- | Game-specific components for roguelike rendering.
--
-- This module provides specialized components for common roguelike game display
-- patterns:
--
-- * Tile-based map rendering with viewport
-- * Entity layer for rendering game objects
-- * Utilities for managing map viewports and glyph information
--
-- These components are designed to work with tile-based game maps and handle
-- common patterns like centering the view on the player.
--
-- Note that the design tries to be as unobstrusive as possible when it comes to
-- your own datatypes. We make the least amount of assumptions on the way you
-- store tilemaps or entities. Assumptions in the basic components are:
--
-- * Entities are stored in a `Foldable` datatype;
-- * Tiles are stored in a way that lets you access them by `Cell`.
-- * You are using a coordinate system that is top-left based (no negative
-- coords).
--
-- Typical use case is to start by a `multiLayeredGrid`, which will then compute
-- the viewport from a focus point (usually player position), and render items
-- that should be rendered on each subsequent layer.  `gridTile` and
-- `entitiesLayer` all expect a `MapViewport`.  `multiLayeredGrid` passes this
-- viewport around.
--
-- Important note: the basic components offered here _do not_ support rendering
-- outside of tiles. So smooth entity movement is not possible right now. This
-- might be added later on though.
--
-- Example with a basic rendering:
--
-- @
--
-- data YourTileType = Floor | Wall -- .. and so on
-- data LivingThings = Player | Orc | Goblin -- | .. and so on
--
-- tileToGlyphInfo :: YourTileType -> GlyphInfo tileToGlyphInfo = ... -- define
-- your rendering per tile here
--
-- monsterToGlyphInfo :: LivingThings -> GlyphInfo monsterToGlyphInfo = ... --
-- define your monster and player rendering here
--
-- renderGame :: Array (V2 Cell) YourTileType -> V2 Cell -> [(YourMonsterType, V2 Cell)]
-- renderGame tilemap playerPos monsterPos =
--   multiLayeredGrid (V2 100 100) -- If your map is 100x100 playerPos
--   playerPos -- We use the player as a focus point
--   [ gridTile (arbitraryMap !) tileToGlyphInfo
--   , entitiesLayer ((Player, playerPos):monsterPos) (monsterToGlyphInfo . fst) snd
--   ]
--
-- @
--
-- All game components use `V2 Cell` for coordinates; but these are coordinates
-- in your game, not on the console. The components handle conversion to screen
-- coordinates.
module Rogui.Components.Game
  ( -- * Grid layering
    multiLayeredGrid,

    -- * Tile-based map rendering
    gridTile,

    -- * Entity rendering
    entitiesLayer,
    animatedEntitiesLayer,

    -- * Utilities
    GlyphInfo (..),
    MapViewport,
    computeMapViewport,
  )
where

import Rogui.Components.Game.EntitiesLayer (animatedEntitiesLayer, entitiesLayer)
import Rogui.Components.Game.GridTile (gridTile, multiLayeredGrid)
import Rogui.Components.Game.Utils (GlyphInfo (..), MapViewport, computeMapViewport)
