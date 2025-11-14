{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}

-- | There is no right or wrong way to setup your modules, but
-- rogues tend to get messy quickly, so it's generally a good
-- idea to have a structure. We use the "game mode" notion as
-- our main driver: most of the logic is stored in `GameModes`
-- submodules, with each mode getting its `Rendering` and `Events
-- module (unless it's very simple like `Inventory`).
module RogueHarvest.Game
  ( renderGame,
    handleGameEvents,
    baseState,
  )
where

import Control.Applicative ((<|>))
import Control.Monad.Random (MonadRandom)
import Data.Array.IArray
import Data.Foldable
import qualified Data.Map as M
import Data.Maybe (catMaybes, isJust)
import qualified Data.Sequence as Seq
import Lens.Micro.Platform
import Linear (V2 (..))
import RogueHarvest.Components.ConfirmDialog
import RogueHarvest.Constants
import RogueHarvest.Entities (emptyEntityMap, processDay)
import RogueHarvest.GameModes.Help (renderHelpMenu)
import RogueHarvest.GameModes.Inventory
import RogueHarvest.GameModes.Menu (handleMenuEvents, renderMainMenu)
import RogueHarvest.GameModes.Playing
import RogueHarvest.GameModes.Shopping
import RogueHarvest.Types
import RogueHarvest.Utils
import Rogui.Application.Event
import Rogui.Components
import Rogui.FocusRing
import Rogui.Graphics
import Rogui.Types
import qualified SDL

-- A new game will start with this state
baseState :: RogueHarvest
baseState =
  let farmMap = defaultFarmMap
   in RogueHarvest
        { _farm = farmMap,
          _playerPos = V2 1 5,
          _currentMode = Menu (focusRing [MenuNew, MenuQuit]),
          _money = 100,
          _energy = 100,
          _inventory = [(Hoe, 1), (Watercan, 1)],
          _wielding = Nothing,
          _entities = emptyEntityMap farmMap,
          _logs = []
        }

-- We don't do fancy procedural generation in this example, we
-- use a default initialised map
defaultFarmMap :: FarmMap
defaultFarmMap =
  let generateTile (V2 0 0) = House NW
      generateTile (V2 1 0) = House N
      generateTile (V2 2 0) = House NE
      generateTile (V2 0 1) = House W
      generateTile (V2 1 1) = House C
      generateTile (V2 2 1) = House E
      generateTile (V2 0 2) = House SW
      generateTile (V2 1 2) = House S
      generateTile (V2 2 2) = House SE
      generateTile _ = Meadow
   in genArray (V2 0 0, V2 60 60) generateTile

-- This is our main rendering logic.
--

-- * At the lowest z-level, we drop the game grid.

-- * We always draw logs and status bar.

-- * The rest is modals that get rendered if the game state requires it.

--
-- `catMaybes` is a good tool to do this type of things.
renderGame :: ConsoleDrawers Consoles Brushes Names RogueHarvest
renderGame brushes state = case _currentMode state of
  Menu ring -> [(Just Root, Just Charset, renderMainMenu ring)]
  _ ->
    catMaybes
      [ Just (Just GridConsole, Just Charset, renderGrid brushes state),
        Just (Just LogConsole, Just SmallCharset, renderLogs state),
        Just
          (Just StatusBar, Just SmallCharset, renderStatus state),
        (\sst -> (Just ModalShop, Just SmallCharset, renderTrading sst)) <$> (state ^? currentMode . _Trading),
        (\ls -> (Just ModalShop, Just SmallCharset, renderInventory ls state)) <$> (state ^? currentMode . _Inventory),
        if isJust (state ^? currentMode . _Help) then Just (Just ModalShop, Just SmallCharset, renderHelpMenu) else Nothing,
        (\dialogState -> (Just ModalLayer, Just SmallCharset, renderSleepDialog (state ^. playerPos) dialogState)) <$> (state ^? currentMode . _SleepDialog)
      ]

-- We use rogui's `messageLog`.
renderLogs :: RogueHarvest -> Component Names
renderLogs RogueHarvest {_logs} =
  vBox [messageLog (toList $ Seq.drop (Seq.length _logs - 10) _logs) (V2 0 0)]

-- We distribute events depending on the current game mode.
-- Some events are mutualised: switching game mode, for instance,
-- is universal.
handleGameEvents :: (MonadRandom m) => EventHandler m RogueHarvest RHEvents Names
handleGameEvents s@RogueHarvest {..} e = case _currentMode of
  Menu ring -> handleMenuEvents ring s e
  Playing p -> handlePlayingEvents p s e <|> handleRHEvents s e
  Trading sst -> handleTradingEvents sst s e <|> handleRHEvents s e
  Inventory ls -> handleInventoryEvents ls s e <|> handleRHEvents s e
  SleepDialog dialogState -> handleSleepDialogEvents dialogState s e <|> handleRHEvents s e
  _ -> handleRHEvents s e

-- Here, we are faced with a small design issue. A lot of game events are
-- fired in some modes, but are relevant for others, or could be fired in
-- different contexts. E.g., the `Wield` event can be fired from Inventory,
-- but also from the Playing mode (when pressing `W`, we wield Nothing).
-- So it's hard to figure where to put this logic, which might end up
-- taking a lot of place in your code. On the other hand, it provides
-- a centralized place for "main" events.
--
-- For a more complicated project, it might be wise to group these
-- under a variety of properly named helpers, so that you can clearly
-- follow track.
handleRHEvents :: (MonadRandom m) => EventHandler m RogueHarvest RHEvents Names
handleRHEvents RogueHarvest {_farm, _currentMode, _playerPos, _wielding, _entities} = \case
  (KeyDown (KeyDownDetails _ (KeyDetails SDL.KeycodeEscape _))) -> leaveMode _currentMode
  (AppEvent (SwitchMode m)) -> modifyState (\s -> s {_currentMode = m})
  (AppEvent (FinalisePurchase pdt bill')) -> modifyState (applyPurchase pdt bill') >> fireAppEvent (SwitchMode (Playing Walking))
  (AppEvent (FinaliseSale sdt bill')) -> modifyState (applySale sdt bill') >> fireAppEvent (SwitchMode (Playing Walking))
  (AppEvent (Wield item)) -> do
    case item of
      Just i -> addLog [(bnw, "You now wield a "), (Colours (Just yellow) (Just black), show i)]
      Nothing -> addLog [(bnw, "Your hands are now free")]
    modifyState (\s -> s {_currentMode = Playing Walking, _wielding = item})
  (AppEvent UseWieldedItem) -> setAiming _wielding
  (AppEvent DayEnd) -> do
    addLog [(bnw, "A new day begins!")]
    (morningEntities, morningFarm) <- processDay _entities _farm
    modifyState (\s -> s {_energy = 100, _currentMode = Playing Walking, _entities = morningEntities, _farm = morningFarm})
  _ -> unhandled

applyPurchase :: PurchasingMode -> Int -> RogueHarvest -> RogueHarvest
applyPurchase PurchasingMode {..} bill' rh =
  let fused = M.unionWith (+) _currentPurchases
   in rh
        & money
        -~ bill'
        & inventory
        %~ fused

applySale :: SellingMode -> Int -> RogueHarvest -> RogueHarvest
applySale SellingMode {..} bill' rh =
  let fused = M.filter (> 0) . M.unionWith (-) (M.mapKeys Product _currentSale)
   in rh
        & money
        +~ bill'
        & inventory
        %~ fused

leaveMode :: (Monad m) => GameMode -> EventHandlerM m RogueHarvest RHEvents Names ()
leaveMode _ = fireAppEvent (SwitchMode (Playing Walking))

handleSleepDialogEvents :: (Monad m) => ConfirmDialogState -> EventHandler m RogueHarvest RHEvents Names
handleSleepDialogEvents dialogState =
  let updater newState s = s {_currentMode = SleepDialog newState}
      onSelect choice = case choice of
        Yes -> do
          addLog [(bnw, "You go to sleep...")]
          fireAppEvent DayEnd
        No -> do
          addLog [(bnw, "You decide to stay awake.")]
          fireAppEvent (SwitchMode (Playing Walking))
   in handleConfirmDialogEvent SleepYesButton SleepNoButton dialogState updater onSelect

renderStatus :: RogueHarvest -> Component Names
renderStatus RogueHarvest {..} =
  let progressBarDefinition =
        ProgressBarDefinition
          { minimumValue = 0,
            maximumValue = 100,
            value = _energy,
            coloursFilled = Colours (Just green) (Just black),
            coloursUnfilled = Colours (Just darkerGreen) (Just black),
            glyphFilled = fullBlock,
            glyphUnfilled = lightShade
          }
   in hBox
        [ hSize (Fixed 10) $ label (show _money <> " $") TLeft bnw,
          hSize (Fixed 30) $ label (maybe "Empty handed" (\i -> "Wielding " <> nameItem i) _wielding) TLeft bnw,
          progressBar progressBarDefinition
        ]