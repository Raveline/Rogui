{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

-- | The main types used throughout the game.
-- Since we're going to need quite a few deep records / map / array updating,
-- we're using `microlenses` to help us.
--
-- You could use any other lens-like solution, like optics plus its generic
-- derivation, for instance.
--
-- TH generated lenses generate _a lot_ of stuff to export, so we have
-- opted for exporting _everything_ here.
module RogueHarvest.Types
  ( module RogueHarvest.Types,
    module RogueHarvest.Types.Item,
    module RogueHarvest.Types.Entity,
  )
where

import Data.Array
import Data.Bits (shiftR, xor)
import Data.Char (ord)
import qualified Data.Map as M
import Data.Sequence (Seq)
import Lens.Micro.Pro.TH
import Lens.Micro.TH
import Linear (V4 (..))
import Linear.V2 (V2 (..))
import RogueHarvest.Components.ConfirmDialog (ConfirmDialogState)
import RogueHarvest.Types.Entity
import RogueHarvest.Types.Item
import Rogui.Application.Event
import Rogui.Components.Game.Utils (GlyphInfo (..))
import Rogui.Components.List
import Rogui.Components.MessageLog (LogMessage)
import Rogui.FocusRing
import Rogui.Graphics

-- The classic Rogui triad: Consoles, Brush and Names

data Consoles = Root | GridConsole | LogConsole | ModalShop | ModalHelp | StatusBar | ModalLayer
  deriving (Eq, Ord, Show)

data Brushes = Charset | SmallCharset
  deriving (Eq, Ord, Show)

data Names = InventoryList | ShoppingList | ValidateButton | SleepYesButton | SleepNoButton | HelpGrid | MenuNew | MenuQuit | LoggingArea
  deriving (Eq, Ord, Show)

-- We call the game state "modes". Modes come with their own record to keep track
-- of their local states.

-- This mode is used when player is "aiming" at something (tiles around to till,
-- plant, water, harvest..). The underlying action is presumed from the object
-- wielded by the player (or not; no value in `withObject` means player is
-- harvesting crops).
data AimingMode = AimingMode
  { _potentialCells :: [V2 Cell],
    _withObject :: Maybe Item,
    _currentTarget :: Maybe (V2 Cell)
  }

makeLenses ''AimingMode

-- Modes can be subdivided: for instant, the main "Playing" mode is thus
-- divided between walking and aiming.
data PlayingMode
  = Walking
  | Aiming AimingMode

makePrisms ''PlayingMode

-- We can also try to group things that have a lot in common.
-- In RogueHarvest, the player can buy seeds and sell crops. Even though
-- there are slight differences in the logic at play, by factorizing
-- as much as possible, we'll have an easier time handling rendering
-- and
newtype PurchasingMode = PurchasingMode
  { _currentPurchases :: M.Map Item Int
  }

makeLenses ''PurchasingMode

data SellingMode = SellingMode
  { _currentSale :: M.Map Crop Int,
    _maximumSale :: M.Map Crop Int
  }

makeLenses ''SellingMode

data TradingSubmode
  = Purchasing PurchasingMode
  | Selling (Maybe SellingMode)

makePrisms ''TradingSubmode

data TradingMode = TradingMode
  { _submode :: TradingSubmode,
    _tradingRing :: FocusRing Names,
    _tradingListState :: ListState,
    _bill :: Int
  }

makeLenses ''TradingMode

mkPurchasingState :: TradingMode
mkPurchasingState = mkTradingMode $ Purchasing (PurchasingMode mempty)

mkSellingState :: M.Map Item Int -> TradingMode
mkSellingState inventory =
  let foldProductOnly (Product c) a acc = (c, a) : acc
      foldProductOnly _ _ acc = acc
      onlyProducts = M.fromList . M.foldrWithKey foldProductOnly [] $ inventory
   in mkTradingMode . Selling $
        if null onlyProducts
          then Nothing
          else
            Just $
              SellingMode
                { _currentSale = 0 <$ onlyProducts,
                  _maximumSale = onlyProducts
                }

mkTradingMode :: TradingSubmode -> TradingMode
mkTradingMode submode' =
  TradingMode
    { _submode = submode',
      _bill = 0,
      _tradingListState = ListState {selection = Just 0, scrollOffset = 0},
      _tradingRing = focusRing [ShoppingList, ValidateButton]
    }

-- And here is the definition of the main modes the game can be in.
-- Using Prisms is quite helpful here when we need to make setters
-- for the substate of each mode.
data GameMode
  = Menu (FocusRing Names)
  | Playing PlayingMode
  | Trading TradingMode
  | Inventory ListState
  | SleepDialog ConfirmDialogState
  | Help

makePrisms ''GameMode

-- Here are all the custom events we support in the game.
data RHEvents
  = DayEnd
  | TilledAt (V2 Cell)
  | WaterAt (V2 Cell)
  | Direction (V2 Cell)
  | FinalisePurchase PurchasingMode Int
  | FinaliseSale SellingMode Int
  | NewGame
  | SwitchMode GameMode
  | UseWieldedItem
  | Wield (Maybe Item)

-- This is the canonical "tile type". Since we want to keep
-- things simple, the house is encoded as tiles.
-- The MultiTileLayout trick is something that could be used
-- to represent other things (e.g., walls which representation
-- should change depending on the neighbouring walls).
data Tile
  = Meadow
  | Field Int -- After X days, fields return to meadow
  | House MultiTileLayout
  deriving (Eq)

data MultiTileLayout = NW | N | NE | W | C | E | SW | S | SE
  deriving (Eq)

-- I find that arrays are very convenient thanks to their
-- arbitraries indexes.
type FarmMap = Array (V2 Cell) Tile

-- This being a simple demo project, we didn't go full ECS.
-- Rather, we keep a record of potential entities sitting on
-- a given tile. This is not the most elegant or efficient
-- solution, and I wouldn't recommend it for a "serious"
-- roguelike, but it works quite well in this use-case.
type EntityMap = Array (V2 Cell) CellContents

-- And finally, our main state datatype. Quite a few
-- fields could be packaged in a "Player" datatype,
-- but it would be a bit overkill.
data RogueHarvest = RogueHarvest
  { _farm :: FarmMap,
    _playerPos :: V2 Cell,
    _currentMode :: GameMode,
    _money :: Int,
    _energy :: Int,
    _inventory :: M.Map Item Int,
    _wielding :: Maybe Item,
    _entities :: EntityMap,
    _logs :: Seq LogMessage
  }

makeLenses ''RogueHarvest

-- Characters used for meadow variation to create organic texture
meadowChars :: [Char]
meadowChars = [',', ';', '.', '\'', '"', '`'] <> replicate 8 ' '

-- Hashing with prime multipliers and bit mixing for a bit of pseudo-randomness
hashPosition :: V2 Cell -> Int
hashPosition (V2 (Cell x) (Cell y)) =
  let h1 = x * 374761393 + y * 668265263
      h2 = (h1 `xor` (h1 `shiftR` 13)) * 1274126177
      h3 = h2 `xor` (h2 `shiftR` 16)
   in abs h3

tileToGlyphInfo :: V2 Cell -> Tile -> GlyphInfo
tileToGlyphInfo pos = \case
  Meadow ->
    let variant = hashPosition pos `mod` length meadowChars
        char = meadowChars !! variant
        -- Use a lighter, more yellowish green for better contrast
        meadowForeground = V4 180 255 100 255
     in GlyphInfo (ord char) (Colours (Just meadowForeground) (Just darkGreen)) []
  (Field _) -> GlyphInfo (ord ' ') (Colours Nothing (Just $ V4 90 80 60 255)) []
  (House NW) -> GlyphInfo arrowheadTop (Colours (Just red) (Just darkGreen)) []
  (House N) -> GlyphInfo arrowheadTop (Colours (Just red) (Just darkGreen)) []
  (House NE) -> GlyphInfo arrowheadTop (Colours (Just red) (Just darkGreen)) []
  (House W) -> GlyphInfo (ord ' ') (Colours Nothing (Just red)) []
  (House C) -> GlyphInfo (ord ' ') (Colours Nothing (Just red)) []
  (House E) -> GlyphInfo (ord ' ') (Colours Nothing (Just red)) []
  (House SW) -> GlyphInfo (ord ' ') (Colours Nothing (Just red)) []
  (House S) -> GlyphInfo mediumShade (Colours (Just black) (Just red)) []
  (House SE) -> GlyphInfo (ord ' ') (Colours Nothing (Just red)) []

makeLenses ''CellContents
makeLenses ''PlantState

type RHEventHandler m = EventHandler m RogueHarvest RHEvents Names