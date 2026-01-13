{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- This module handles both buying seeds and selling
-- products.
module RogueHarvest.GameModes.Shopping.Rendering
  ( renderTrading,
    renderSelling,
    purchaseListDefinition,
    sellingListDefinition,
  )
where

import Control.Monad (when)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Linear (V2 (..))
import RogueHarvest.Constants
import RogueHarvest.Types
import Rogui.Components
import Rogui.Components.List
import Rogui.FocusRing
import Rogui.Graphics

-- Since buying and selling share a lot of similarities, we can
-- use a helper to factorize most of the display
tradeWindow :: String -> String -> FocusRing Names -> Component Names -> Component Names
tradeWindow title buttonText ring list' =
  filled black
    . bordered bnw
    $ vBox
      [ vSize (Fixed 2) $ label title TCenter bnw,
        list',
        button ValidateButton buttonText TLeft bnw (invert bnw) (focusGetCurrent ring == Just ValidateButton)
      ]

renderTrading :: TradingMode -> Component Names
renderTrading tm@TradingMode {..} = case _submode of
  (Purchasing pm) -> renderPurchasing tm pm
  (Selling sm) -> renderSelling tm sm

renderPurchasing :: TradingMode -> PurchasingMode -> Component Names
renderPurchasing TradingMode {..} st =
  tradeWindow
    "Buying products"
    ("Purchase (" <> show _bill <> " $)")
    _tradingRing
    (list (purchaseListDefinition st) _tradingListState)

renderSelling :: TradingMode -> Maybe SellingMode -> Component Names
renderSelling TradingMode {..} = \case
  Nothing -> filled black . bordered bnw $ vBox [label "You don't have any products to sell !" TCenter bnw]
  Just sm ->
    tradeWindow
      "Selling products"
      ("Sell (" <> show _bill <> " $)")
      _tradingRing
      (list (sellingListDefinition sm) _tradingListState)

purchaseListDefinition :: PurchasingMode -> ListDefinition Names Item
purchaseListDefinition pm =
  ListDefinition
    { name = ShoppingList,
      items = Seed <$> allCrops,
      renderItem = purchaseLine pm,
      itemHeight = 1,
      wrapAround = False
    }

sellingListDefinition :: SellingMode -> ListDefinition Names (Crop, Int)
sellingListDefinition sst =
  ListDefinition
    { name = ShoppingList,
      items = M.toList (_currentSale sst),
      renderItem = salesLine,
      itemHeight = 1,
      wrapAround = False
    }

purchaseLine :: PurchasingMode -> Item -> Bool -> Component Names
purchaseLine PurchasingMode {..} item =
  let name (Seed c) = show c <> " seeds"
      name _ = ""
   in productLine
        (name item)
        (marginalCost item)
        (fromMaybe 0 $ item `M.lookup` _currentPurchases)

salesLine :: (Crop, Int) -> Bool -> Component Names
salesLine (crop, amount) =
  productLine (show crop) (marginalCost $ Product crop) amount

productLine :: String -> Int -> Int -> Bool -> Component Names
productLine name cost amount focused =
  let draw' = do
        when focused $ setConsoleBackground white
        setColours $ if focused then invert bnw else bnw
        str TLeft name
        pencilAt (V2 25 0)
        str TLeft (show cost)
        pencilAt (V2 28 0)
        str TLeft "$"
        pencilAt (V2 35 0)
        str TLeft "< "
        str TLeft (show amount)
        pencilAt (V2 39 0)
        str TLeft " >"
   in emptyComponent {draw = draw'}