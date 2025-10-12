{-# LANGUAGE DuplicateRecordFields #-}

module Rogui.Graphics.Types
  ( Console (..),
    Brush (..),
    TileSize (..),
  )
where

import SDL (Texture, V2)

-- | A virtual console.
-- Widht, int, position are all in pixels.
data Console = Console
  { width :: Int,
    height :: Int,
    position :: V2 Int
  }

-- | A tileset and metadata used to be displayed on a console.
-- All numerical values are in pixels.
data Brush = Brush
  { -- | Size in width of individual tiles in the tilemap
    tileWidth :: Int,
    -- | Size in height of individual tiles in the tilemap
    tileHeight :: Int,
    -- | Total texture width
    textureWidth :: Int,
    -- | Total texture height
    textureHeight :: Int,
    -- | Texture itself
    brush :: Texture
  }

data TileSize = TileSize
  { pixelWidth :: Int,
    pixelHeight :: Int
  }