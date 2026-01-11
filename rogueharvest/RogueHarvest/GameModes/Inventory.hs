{-# LANGUAGE RecordWildCards #-}

-- This lets player see their inventory.
-- This is less sophisticated than a "real roguelike" inventory,
-- but it should give you a good idea on how one could build
-- an inventory modal with just the `list` component.
--
-- Note that in this context, inventory is only used to pick
-- an item to wield, which simplifies things a bit. Again,
-- in a real roguelike, there would be more interactions
-- to handle.
module RogueHarvest.GameModes.Inventory
  ( renderInventory,
    handleInventoryEvents,
  )
where

import Control.Applicative
import Control.Monad
import qualified Data.Map as M
import RogueHarvest.Constants
import RogueHarvest.Types
import Rogui.Application.Event
import Rogui.Components
import Rogui.Components.List
import Rogui.Graphics

-- Lists need a ListDefinition, and it's convenient to have
-- a function to produce it from state, since you're going
-- to need it both for rendering and for event handling.
--
-- Since player inventory is handled as a `Map`, we picked
-- the `toList` representation for maps (so a list of pairs)
-- as the underlying item type of our list.
inventoryDefinition :: RogueHarvest -> ListDefinition Names (Item, Int)
inventoryDefinition RogueHarvest {..} =
  ListDefinition
    { name = InventoryList,
      renderItem = renderInventoryLine,
      wrapAround = True,
      items = M.toList _inventory,
      itemHeight = Cell 1
    }

-- The list component demands a renderer that morphs
-- list items into components, let's produce it.
renderInventoryLine :: (Item, Int) -> Bool -> Component Names
renderInventoryLine (item, qty) selected =
  let draw' = do
        -- Switch colours when focused
        when selected $ setConsoleBackground white
        setColours $ if selected then invert bnw else bnw

        -- Print object name, plus a quantity if there is more than one
        str TLeft (nameItem item)
        when (qty > 1) $
          str TLeft (" (" <> show qty <> " )")
   in emptyComponent {draw = draw'}

-- Usual chain of handlers: first, we use the default list handler, and
-- since we want some interactivity, we add the `wieldOnEnter` handler
-- that will trigger the fact that the selected item is wielded by the
-- player when enter or return keys are pressed.
handleInventoryEvents :: (Monad m) => ListState -> EventHandler m RogueHarvest RHEvents Names
handleInventoryEvents ls rh e =
  handleListEvent (inventoryDefinition rh) ls (\ls' s -> s {_currentMode = Inventory ls'}) rh e <|> wieldOnEnter ls rh e

wieldOnEnter :: (Monad m) => ListState -> EventHandler m RogueHarvest RHEvents Names
wieldOnEnter ls rh e =
  let keyMap item =
        [ (IsNoMod KEnter, \_ _ -> fireAppEvent (Wield item))
        ]
      withItem item = keyPressHandler (keyMap item) rh e
   in withItem . fmap fst . getCurrentSelection (inventoryDefinition rh) $ ls

-- And finally, our renderer, which is quite simple.
renderInventory :: ListState -> RogueHarvest -> Component Names
renderInventory ls rh =
  filled black . bordered bnw . padded 2 $ list (inventoryDefinition rh) ls