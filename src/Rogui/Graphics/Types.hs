{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Main types used throughout the library.  Pixel and Tiles newtypes allow to
-- ensure we are always measuring the right unit. Operators provided use
-- mnemonics: a '.' sign means pixel, a '=' means tiles. The final sign
-- indicates the result type of the operation. E.g.: `.*.=` means multiply pixel
-- unit by pixel units to find a tile unit. For people who hate custom
-- operators, full named functions are given.
module Rogui.Graphics.Types
  ( Console (..),
    Brush (..),
    TileSize (..),
    Pixel (..),
    Tile (..),
    (.*=.),
    pixelTimesTiles,
    (./.=),
    pixelDivPixelToTile,
    fromBrush,
  )
where

import Data.Ix (Ix)
import SDL (Texture, V2)

newtype Pixel = Pixel {getPixel :: Int}
  deriving newtype (Num, Integral, Real, Ord, Eq, Enum, Show)

newtype Tile = Tile {getTile :: Int}
  deriving newtype (Num, Integral, Real, Ord, Eq, Enum, Show, Ix)

(.*=.) :: Pixel -> Tile -> Pixel
(Pixel p) .*=. (Tile t) = Pixel (p * t)

pixelTimesTiles :: Pixel -> Tile -> Pixel
pixelTimesTiles = (.*=.)

(./.=) :: Pixel -> Pixel -> Tile
(Pixel p) ./.= (Pixel p2) = Tile $ p `div` p2

pixelDivPixelToTile :: Pixel -> Pixel -> Tile
pixelDivPixelToTile = (./.=)

-- | A virtual console.
-- Width, int, position are all in pixels.
data Console = Console
  { width :: Pixel,
    height :: Pixel,
    position :: V2 Pixel
  }
  deriving (Show, Eq)

-- | A tileset and metadata used to be displayed on a console.
-- All numerical values are in pixels.
data Brush = Brush
  { -- | Size in width of individual tiles in the tilemap
    tileWidth :: Pixel,
    -- | Size in height of individual tiles in the tilemap
    tileHeight :: Pixel,
    -- | Total texture width
    textureWidth :: Pixel,
    -- | Total texture height
    textureHeight :: Pixel,
    -- | Texture itself
    brush :: Texture
  }

data TileSize = TileSize
  { pixelWidth :: Pixel,
    pixelHeight :: Pixel
  }

fromBrush :: Brush -> TileSize
fromBrush Brush {..} = TileSize tileWidth tileHeight