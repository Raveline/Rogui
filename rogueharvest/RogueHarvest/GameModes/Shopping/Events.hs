{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- It's a bit harder to factorize event handling between purchase and sell menu,
-- but it's mostly doable. When you have vastly similar UIs for two different
-- data processes (here, buying vs. selling), it's a good idea to factorize
-- the ui state (here, the focus ring and the listStates).
module RogueHarvest.GameModes.Shopping.Events
  ( handleTradingEvents,
  )
where

import Control.Applicative
import Control.Monad (when)
import Data.Foldable (traverse_)
import qualified Data.Map.Strict as M
import Data.Maybe
import Lens.Micro.Platform
import RogueHarvest.GameModes.Shopping.Rendering (purchaseListDefinition, sellingListDefinition)
import RogueHarvest.Types
import Rogui.Application
import Rogui.Components (handleButtonEvent)
import Rogui.Components.List
import Rogui.FocusRing
import qualified SDL

handleTradingEvents :: (Monad m) => TradingMode -> EventHandler m RogueHarvest RHEvents Names
handleTradingEvents tm@TradingMode {..} =
  let handleTradingEvents' listDef finalizer tradeChangeHandler s e =
        let handleFocusedEvent = case focusGetCurrent _tradingRing of
              (Just ShoppingList) ->
                handleListEvent listDef e _tradingListState updateListState
                  <|> handleBillChange (tradeChangeHandler _tradingListState) s e
              (Just ValidateButton) -> handleButtonEvent (AppEvent (finalizer _bill)) s e
              _ -> unhandled
         in handleFocusedEvent <|> handleFocusChange tm listDef s e
   in case _submode of
        Purchasing pm -> handleTradingEvents' (purchaseListDefinition pm) (FinalisePurchase pm) (attemptPurchaseBillChange pm)
        Selling (Just st) -> handleTradingEvents' (sellingListDefinition st) (FinaliseSale st) (attemptSaleBillChange st)
        _ -> const . const $ unhandled

updateListState :: ListState -> RogueHarvest -> RogueHarvest
updateListState ls = currentMode . _Trading . tradingListState .~ ls

updatePurchases :: M.Map Item Int -> RogueHarvest -> RogueHarvest
updatePurchases cp = currentMode . _Trading . submode . _Purchasing . currentPurchases .~ cp

updateSale :: M.Map Crop Int -> RogueHarvest -> RogueHarvest
updateSale cs = currentMode . _Trading . submode . _Selling . _Just . currentSale .~ cs

handleBillChange :: (Monad m) => ((Int -> Int) -> EventHandler m RogueHarvest RHEvents Names) -> EventHandler m RogueHarvest RHEvents Names
handleBillChange changeBill =
  let keyMaps =
        [ ((SDL.KeycodeLeft, []), changeBill (\x -> x - 1)),
          ((SDL.KeycodeKP4, []), changeBill (\x -> x - 1)),
          ((SDL.KeycodeRight, []), changeBill (+ 1)),
          ((SDL.KeycodeKP6, []), changeBill (+ 1)),
          -- Move cursor to the purchase button if user press enter
          ((SDL.KeycodeReturn, []), \_ _ -> fireEvent FocusNext)
        ]
   in keyPressHandler keyMaps

computeBill :: M.Map Item Int -> Int
computeBill =
  let billProducts i amount total = total + (marginalCost i * amount)
   in M.foldrWithKey billProducts 0

attemptSaleBillChange :: (Monad m) => SellingMode -> ListState -> (Int -> Int) -> EventHandler m RogueHarvest RHEvents Names
attemptSaleBillChange sst@SellingMode {..} ls f _ _ =
  traverse_
    ( \(product', _amount) -> do
        let maxAllowedSale = fromMaybe 0 $ product' `M.lookup` _maximumSale
            change upon
              | f upon < 0 = 0
              | f upon > maxAllowedSale = maxAllowedSale
              | otherwise = f upon
            withNewSale :: M.Map Crop Int
            withNewSale = M.alter (fmap change) product' _currentSale
            newBill = computeBill . M.mapKeys Product $ withNewSale
        modifyState (updateSale withNewSale)
        modifyState (currentMode . _Trading . bill .~ newBill)
    )
    $ getCurrentSelection (sellingListDefinition sst) ls

attemptPurchaseBillChange :: (Monad m) => PurchasingMode -> ListState -> (Int -> Int) -> EventHandler m RogueHarvest RHEvents Names
attemptPurchaseBillChange sst@PurchasingMode {..} ls f RogueHarvest {..} _ =
  traverse_
    ( \product' -> do
        let change (Just upon)
              | f upon < 0 = pure 0
              | f upon > 99 = pure 99
              | otherwise = Just $ f upon
            change Nothing = change (Just 0)
            withNewShopping = M.alter change product' _currentPurchases
            -- we still need to verify (and update) the price
            newBill = computeBill withNewShopping
        when (newBill <= _money) $ do
          modifyState (updatePurchases withNewShopping)
          modifyState (currentMode . _Trading . bill .~ newBill)
    )
    $ getCurrentSelection (purchaseListDefinition sst) ls

handleFocusChange ::
  (Monad m) =>
  TradingMode ->
  ListDefinition Names b ->
  EventHandler m RogueHarvest RHEvents Names
handleFocusChange TradingMode {..} listDef _ event =
  let handleFocus focusChanger focusDir = do
        let newFocus = focusChanger _tradingRing
        modifyState (currentMode . _Trading . tradingRing .~ newFocus)
        when (focusGetCurrent newFocus == Just ShoppingList) $ do
          listReceiveFocus listDef _tradingListState focusDir updateListState
   in case event of
        FocusNext -> handleFocus focusNext FromNext
        FocusPrev -> handleFocus focusPrev FromPrev
        _ -> unhandled