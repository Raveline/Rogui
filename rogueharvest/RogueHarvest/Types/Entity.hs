{-# LANGUAGE LambdaCase #-}

module RogueHarvest.Types.Entity
  ( PlantState (..),
    CellContents (..),
    GrowthStage (..),
    renderCrop,
    mkGlyphInfo,
    cropGrowthRythm,
  )
where

import Data.Char (ord)
import RogueHarvest.Types.Item (Crop (..))
import Rogui.Components.Game.Utils (GlyphInfo (..))
import Rogui.Graphics (Colours (..), RGBA)
import Rogui.Graphics.Constants

data GrowthStage = Planted | Seedling | Growing | Grown
  deriving (Eq, Ord, Bounded, Enum, Show)

data PlantState = PlantState
  { _plantCrop :: Crop,
    _growthStage :: GrowthStage,
    _daysGrown :: Int,
    _watered :: Bool
  }
  deriving (Eq, Show)

-- | Helper function to create a GlyphInfo from a character and foreground color
mkGlyphInfo :: Char -> RGBA -> GlyphInfo
mkGlyphInfo c rgba = GlyphInfo (ord c) (Colours (Just rgba) Nothing) []

renderCrop :: Crop -> GrowthStage -> GlyphInfo
renderCrop Carrot Planted = mkGlyphInfo '.' orange
renderCrop Carrot Seedling = mkGlyphInfo ',' orange
renderCrop Carrot Growing = mkGlyphInfo 'o' orange
renderCrop Carrot Grown = mkGlyphInfo '^' orange
renderCrop Melon Planted = mkGlyphInfo '.' green
renderCrop Melon Seedling = mkGlyphInfo '-' green
renderCrop Melon Growing = mkGlyphInfo 'o' green
renderCrop Melon Grown = mkGlyphInfo 'O' green
renderCrop Potato Planted = mkGlyphInfo '.' gold
renderCrop Potato Seedling = mkGlyphInfo '^' gold
renderCrop Potato Growing = mkGlyphInfo 'o' gold
renderCrop Potato Grown = mkGlyphInfo '*' gold
renderCrop Wheat Planted = mkGlyphInfo '.' yellow
renderCrop Wheat Seedling = mkGlyphInfo '^' yellow
renderCrop Wheat Growing = mkGlyphInfo '/' yellow
renderCrop Wheat Grown = mkGlyphInfo '|' yellow

cropGrowthRythm :: Crop -> Int
cropGrowthRythm = \case
  Carrot -> 2
  Melon -> 2
  Potato -> 1
  Wheat -> 3

newtype CellContents = CellContents
  { _cellPlant :: Maybe PlantState
  }
  deriving (Eq, Show)
