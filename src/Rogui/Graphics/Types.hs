{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Main types used throughout the library.  Pixel and Cell newtypes allow to
-- ensure we are always measuring the right unit. Operators provided use
-- mnemonics: a '.' sign means pixel, a '=' means grid cells. The final sign
-- indicates the result type of the operation. E.g.: `.*.=` means multiply pixel
-- unit by pixel units to find a cell count. For people who hate custom
-- operators, full named functions are given.
module Rogui.Graphics.Types
  ( Console (..),
    Brush (..),
    TileSize (..),
    Pixel (..),
    Cell (..),
    (.*=.),
    pixelTimesCells,
    (./.=),
    pixelDivPixelToCells,
    fromBrush,
  )
where

import Data.Ix (Ix)
import SDL (Texture, V2)

newtype Pixel = Pixel {getPixel :: Int}
  deriving newtype (Num, Integral, Real, Ord, Eq, Enum, Show)

newtype Cell = Cell {getCell :: Int}
  deriving newtype (Num, Integral, Real, Ord, Eq, Enum, Show, Ix)

(.*=.) :: Pixel -> Cell -> Pixel
(Pixel p) .*=. (Cell c) = Pixel (p * c)

pixelTimesCells :: Pixel -> Cell -> Pixel
pixelTimesCells = (.*=.)

(./.=) :: Pixel -> Pixel -> Cell
(Pixel p) ./.= (Pixel p2) = Cell $ p `div` p2

pixelDivPixelToCells :: Pixel -> Pixel -> Cell
pixelDivPixelToCells = (./.=)

-- | A virtual console.
-- Width, height, position are all in pixels.
-- TileSize indicates the expected tile dimensions for this console.
data Console = Console
  { width :: Pixel,
    height :: Pixel,
    position :: V2 Pixel,
    tileSize :: TileSize
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
  deriving (Show, Eq)

fromBrush :: Brush -> TileSize
fromBrush Brush {..} = TileSize tileWidth tileHeight