{-# LANGUAGE RecordWildCards #-}

-- | This is of course a rather basic and clunky system, not a proper
-- ECS. This project a simple demo, so it doesn't need something too
-- sophisticated (also, ECS are often overkill for roguelikes).
module RogueHarvest.Entities
  ( -- * Creating empty entity map
    emptyEntityMap,
    emptyCellContents,

    -- * Planting and growing
    plantSeed,
    harvestPlant,
    waterPlant,

    -- * Time management
    processDay,

    -- * Queries
    isHarvestable,
    isPlantable,
    isWaterable,
    isTillable,
  )
where

import Control.Monad.Random
import Data.Array.Base
import Data.Bifunctor
import Data.Maybe
import Lens.Micro.Platform
import Linear.V2 (V2)
import RogueHarvest.Constants
import RogueHarvest.Types hiding (entities)
import RogueHarvest.Utils (addLog)
import Rogui.Application hiding (repeat)
import Rogui.Application.Event.Types (liftApp)
import Rogui.Graphics (Cell)

-- | Create an empty entity map with the same bounds as the farm
emptyEntityMap :: FarmMap -> EntityMap
emptyEntityMap farmMap = listArray (bounds farmMap) (repeat emptyCellContents)

-- | Empty cell contents
emptyCellContents :: CellContents
emptyCellContents =
  CellContents
    { _cellPlant = Nothing
    }

-- | Plant a seed at a location, returns updated entity map
plantSeed :: V2 Cell -> Crop -> EntityMap -> EntityMap
plantSeed pos crop entities =
  let newPlant =
        PlantState
          { _plantCrop = crop,
            _growthStage = Planted,
            _daysGrown = 0,
            _watered = False
          }
   in entities & ((ix pos . cellPlant) ?~ newPlant)

-- | Harvest a plant if it's ready, returns (harvested crop, updated map)
harvestPlant :: V2 Cell -> EntityMap -> Maybe (Crop, EntityMap)
harvestPlant pos entities =
  let contents = entities ! pos
   in case _cellPlant contents of
        Just PlantState {..}
          | _growthStage == Grown ->
              Just (_plantCrop, entities // [(pos, contents {_cellPlant = Nothing})])
        _ -> Nothing

-- | Water a plant at a location
waterPlant :: V2 Cell -> EntityMap -> (Maybe Crop, EntityMap)
waterPlant pos entities =
  let contents = entities ! pos
   in case _cellPlant contents of
        Just crop -> (Just (crop ^. plantCrop), entities & ix pos . cellPlant . _Just . watered .~ True)
        Nothing -> (Nothing, entities)

-- | Check if a plant is harvestable
isHarvestable :: EntityMap -> V2 Cell -> Bool
isHarvestable entities pos =
  case _cellPlant (entities ! pos) of
    Just PlantState {..} -> _growthStage >= Grown
    Nothing -> False

isWaterable :: EntityMap -> V2 Cell -> Bool
isWaterable entities pos =
  case entities !? pos of
    Just CellContents {..} -> maybe False (not . _watered) _cellPlant
    Nothing -> False

isPlantable :: FarmMap -> EntityMap -> V2 Cell -> Bool
isPlantable tiles entities pos =
  case (entities !? pos, tiles !? pos) of
    (Just CellContents {..}, Just (Field _)) -> isNothing _cellPlant
    _ -> False

isTillable :: FarmMap -> EntityMap -> V2 Cell -> Bool
isTillable tiles entities pos =
  case (entities !? pos, tiles !? pos) of
    (Just CellContents {..}, Just Meadow) -> isNothing _cellPlant
    _ -> False

processDay :: (MonadRandom m) => EntityMap -> FarmMap -> EventHandlerM m RogueHarvest RHEvents Names (EntityMap, FarmMap)
processDay entities farm' = do
  let processPlant cc@(CellContents Nothing) = pure cc
      processPlant cc@(CellContents (Just p)) = do
        gp <- growPlant p
        pure $ cc {_cellPlant = gp}
      hasPlantAt p = isJust (entities ^? ix p . cellPlant . _Just)
      withPlantInfo = listArray (bounds farm') $ first hasPlantAt <$> assocs farm'
  plants <- traverse processPlant entities
  tiles <- traverse (uncurry processTile) withPlantInfo
  pure (plants, tiles)

-- Tilled ground without a plant on it will slowly go back to meadow.
-- There is a counter on till to keep track of this. To add a bit
-- of randomness to this process, the decrement only activates 90%
-- of the time.
processTile :: (MonadRandom m) => Bool -> Tile -> EventHandlerM m RogueHarvest RHEvents Names Tile
processTile False (Field n) = do
  diceRoll <- liftApp $ getRandomR (1, 100 :: Int)
  pure $
    if diceRoll > 90
      then Field n
      else reclaimTilledGround n
processTile _ other = pure other

reclaimTilledGround :: Int -> Tile
reclaimTilledGround n
  | n > 0 = Field $ n - 1
  | otherwise = Meadow

-- Plants grow if they have been watered. Their growth speed depend
-- upon the seed. If they haven't been watered, they have 50% chance
-- of withering (and basically disappear).
-- TODO: add a logging when plant withered
-- TODO: coalesce similar logs
growPlant :: (MonadRandom m) => PlantState -> EventHandlerM m RogueHarvest RHEvents Names (Maybe PlantState)
growPlant ps@PlantState {..} =
  if _watered
    then do
      pure $
        Just $
          ps
            { _growthStage = cropGrow _plantCrop _daysGrown _growthStage,
              _daysGrown = _daysGrown + 1,
              _watered = False
            }
    else do
      coinToss <- liftApp getRandom
      if coinToss
        then pure $ Just ps -- no growth
        else addLog [(bnw, "One crop was not wathered and withered")] >> pure Nothing -- wither and die

cropGrow :: Crop -> Int -> GrowthStage -> GrowthStage
cropGrow _ _ Grown = Grown
cropGrow c nDays gs = if nDays `mod` cropGrowthRythm c == 0 then succ gs else gs