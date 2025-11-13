{-# LANGUAGE LambdaCase #-}

module RogueHarvest.Types.Item
  ( Item (..),
    Crop (..),
    nameItem,
    marginalCost,
    allCrops,
  )
where

data Item
  = Seed Crop
  | Product Crop
  | Hoe
  | Watercan
  deriving (Eq, Ord, Show)

data Crop = Carrot | Melon | Potato | Wheat
  deriving (Eq, Ord, Show, Enum, Bounded)

nameItem :: Item -> String
nameItem = \case
  (Seed c) -> show c <> " seed"
  (Product c) -> show c
  Hoe -> "Hoe"
  Watercan -> "Watercan"

marginalCost :: Item -> Int
marginalCost = \case
  (Seed Carrot) -> 5
  (Seed Melon) -> 15
  (Seed Potato) -> 8
  (Seed Wheat) -> 3
  (Product Carrot) -> 8
  (Product Melon) -> 24
  (Product Potato) -> 10
  (Product Wheat) -> 4
  _ -> 0

allCrops :: [Crop]
allCrops = [Carrot .. maxBound]
