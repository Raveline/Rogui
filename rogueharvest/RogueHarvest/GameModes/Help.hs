{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}

-- The classic help menu. We will implement this one as a grid,
-- mostly to demonstrate real grid usage, and also to show how
-- one can use interactive components without necessary making
-- use of their interactivity, but only to use their rendering.
module RogueHarvest.GameModes.Help
  ( renderHelpMenu,
  )
where

import Data.Maybe (fromMaybe)
import RogueHarvest.Constants
import RogueHarvest.Types (Names (..))
import Rogui.Components
import Rogui.Graphics (Colours (..), TextAlign (..))
import Rogui.Graphics.Constants

data HelpItems
  = Movement
  | Validate
  | Leave
  | Act
  | Purchase
  | Sell
  | Wield
  | Unwield
  deriving (Eq, Ord, Enum, Bounded)

allHelpItems :: [HelpItems]
allHelpItems = [minBound .. maxBound]

helpItemsToStrings :: HelpItems -> [String]
helpItemsToStrings = \case
  Movement -> ["Keypad or arrows", "Move around / pick a selectable tile around you"]
  Validate -> ["<enter>", "Validate a target"]
  Leave -> ["<escape>", "Quit menu / action mode"]
  Act -> ["<space>", "Use your wielded / harvest neighbourging crop"]
  Purchase -> ["s", "Buy seeds"]
  Sell -> ["S", "Sell products"]
  Wield -> ["w", "Wield an item in your inventory"]
  Unwield -> ["W", "Keep your hands free (necessary to harvest !)"]

gridDefinition :: GridDefinition String Names
gridDefinition =
  GridDefinition
    { gridName = HelpGrid,
      gridRows = length allHelpItems,
      gridContent = allHelpItems >>= helpItemsToStrings,
      renderCell = renderGridItem,
      cellWidths = [20, 50],
      cellHeight = 1,
      spacing = 0,
      highlightColours = bnw
    }

renderGridItem :: Int -> Int -> Bool -> Maybe String -> Component Names
renderGridItem col _row _focused content =
  let colours = if col == 0 then Colours (Just gold) (Just black) else bnw
      alignment = if col == 0 then TRight else TLeft
   in label (fromMaybe "" content) alignment colours

renderHelpMenu :: Component Names
renderHelpMenu = filled black . bordered bnw $ grid gridDefinition mkGridState