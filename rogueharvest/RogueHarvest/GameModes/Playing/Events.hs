{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RogueHarvest.GameModes.Playing.Events
  ( handlePlayingEvents,
    setAiming,
  )
where

import Control.Monad (when)
import Data.Array.Base
import Data.Foldable (traverse_)
import Data.List (find)
import Lens.Micro.Platform
import Linear.V2 (V2 (..))
import RogueHarvest.Components.ConfirmDialog (ConfirmChoice (..), ConfirmDialogState (..))
import RogueHarvest.Constants
  ( bnw,
    bny,
    down,
    left,
    ne,
    nw,
    right,
    se,
    sw,
    up,
  )
import RogueHarvest.Entities
import RogueHarvest.Types
import RogueHarvest.Utils (addLog, consumeInventoryItem, getNeightbouringTiles, hasInventoryItem, incrementInventoryItem, modifyEntity, setCurrentMode, setTile, withEnergy)
import Rogui.Application.Event
import Rogui.Components.List (mkListState, selection)
import Rogui.Graphics (Cell)
import qualified SDL

handlePlayingEvents :: (Monad m) => PlayingMode -> EventHandler m RogueHarvest RHEvents Names
handlePlayingEvents pm =
  let inputs = case pm of
        Walking -> handleWalkingKeys <||> handleWalkingRHEvents
        Aiming a -> handleAimingRHEvents a
        WateringAnimation state -> handleWateringAnimationEvents state
   in handleMovementKeys <||> inputs

handleMovementKeys :: (Monad m) => RHEventHandler m
handleMovementKeys =
  let keyMap :: (Monad m) => [(KeyDetailsMatch, RHEventHandler m)]
      keyMap =
        [ (isKC' SDL.KeycodeUp, \_ _ -> fireAppEvent (Direction up)),
          (isKC' SDL.KeycodeKP8, \_ _ -> fireAppEvent (Direction up)),
          (isKC' SDL.KeycodeLeft, \_ _ -> fireAppEvent (Direction left)),
          (isKC' SDL.KeycodeKP4, \_ _ -> fireAppEvent (Direction left)),
          (isKC' SDL.KeycodeKP5, \_ _ -> fireAppEvent (Direction (V2 0 0))),
          (isKC' SDL.KeycodeRight, \_ _ -> fireAppEvent (Direction right)),
          (isKC' SDL.KeycodeKP6, \_ _ -> fireAppEvent (Direction right)),
          (isKC' SDL.KeycodeDown, \_ _ -> fireAppEvent (Direction down)),
          (isKC' SDL.KeycodeKP2, \_ _ -> fireAppEvent (Direction down)),
          (isKC' SDL.KeycodeKP7, \_ _ -> fireAppEvent (Direction nw)),
          (isKC' SDL.KeycodeKP9, \_ _ -> fireAppEvent (Direction ne)),
          (isKC' SDL.KeycodeKP1, \_ _ -> fireAppEvent (Direction sw)),
          (isKC' SDL.KeycodeKP3, \_ _ -> fireAppEvent (Direction se))
        ]
   in keyPressHandler keyMap

handleWalkingKeys :: (Monad m) => RHEventHandler m
handleWalkingKeys =
  let keyMap :: (Monad m) => [(KeyDetailsMatch, RHEventHandler m)]
      keyMap =
        [ (isKC' SDL.KeycodeS, \_ _ -> fireAppEvent (SwitchMode (Trading mkPurchasingState))),
          (isKC SDL.KeycodeS [Shift], \st _ -> fireAppEvent (SwitchMode (Trading . mkSellingState $ _inventory st))),
          (isKC' SDL.KeycodeW, \_ _ -> fireAppEvent (SwitchMode (Inventory mkListState {selection = Just 0}))),
          (isKC SDL.KeycodeW [Shift], \_ _ -> fireAppEvent (Wield Nothing)),
          (isKC' SDL.KeycodeH, \_ _ -> fireAppEvent (SwitchMode Help)),
          (isKC SDL.KeycodeComma [Shift], \_ _ -> fireAppEvent (SwitchMode Help)),
          (isKC' SDL.KeycodeSpace, \_ _ -> fireAppEvent UseWieldedItem)
        ]
   in keyPressHandler keyMap

handleWalkingRHEvents :: (Monad m) => EventHandler m RogueHarvest RHEvents Names
handleWalkingRHEvents RogueHarvest {..} = \case
  (AppEvent (Direction dest)) ->
    let newPos@(V2 x y) = _playerPos + dest
        (V2 minx miny, V2 maxx maxy) = bounds _farm
        inBounds = (x >= minx && y >= miny && x < maxx && y < maxy)
        tileAtDest = _farm ! newPos
        legitDest = case tileAtDest of
          (House S) -> True
          Meadow -> True
          (Field _) -> True
          _ -> False
     in when (inBounds && legitDest) $ do
          modifyState (\s' -> s' {_playerPos = newPos})
          -- Check if player moved to door tile and show sleep dialog
          when (tileAtDest == House S) $
            setCurrentMode (SleepDialog (ConfirmDialogState {focusedChoice = Yes}))
  _ -> unhandled

handleAimingRHEvents :: (Monad m) => AimingMode -> EventHandler m RogueHarvest RHEvents Names
handleAimingRHEvents AimingMode {..} rh = \case
  (KeyDown (KeyDownDetails _ (KeyDetails SDL.KeycodeReturn _ _))) ->
    traverse_ (actionOn rh _withObject) _currentTarget
  (AppEvent (Direction dest)) -> traverse_ (pickSelectedTile dest (rh ^. playerPos)) (rh ^? currentMode . _Playing . _Aiming . potentialCells)
  _ -> unhandled

-- This function encodes all the game logic from player's interaction with the world.
-- Items let us know the nature of the interaction.
actionOn :: (Monad m) => RogueHarvest -> Maybe Item -> V2 Cell -> EventHandlerM m RogueHarvest RHEvents Names ()
actionOn RogueHarvest {..} i target = case i of
  (Just Hoe) ->
    withEnergy 5 $
      setTile target (Field 5)
        >> addLog [(bnw, "You till a piece of land")]
        >> setAiming (Just Hoe)
  (Just Watercan) -> do
    let (wateredCrop, _) = waterPlant target _entities
    withEnergy 2 $ do
      case wateredCrop of
        Just crop -> do
          addLog [(bnw, "You watered a "), (bny, show crop)]
          -- Start the watering animation instead of immediately applying the effect
          setCurrentMode (Playing (WateringAnimation (WateringAnimationState target 0)))
        Nothing -> setCurrentMode (Playing Walking)
  (Just (Seed s)) ->
    withEnergy 3 $ do
      modifyEntity (plantSeed target s)
      addLog [(bnw, "You planted a"), (bny, show s), (bnw, "seed")]
      consumeInventoryItem (Seed s)
      canContinue <- hasInventoryItem (Seed s)
      if canContinue
        then setAiming (Just $ Seed s)
        else addLog [(bnw, "You are out of seeds")] >> setCurrentMode (Playing Walking)
  Nothing ->
    withEnergy 2 $
      let withHarvested (what, newEntities) = do
            addLog [(bnw, "You harvested a"), (bny, show what)]
            modifyState (\s -> s {_entities = newEntities})
            incrementInventoryItem (Product what)
            setAiming Nothing
       in traverse_ withHarvested $ harvestPlant target _entities
  _ -> unhandled

setAiming :: (Monad m) => Maybe Item -> EventHandlerM m RogueHarvest RHEvents Names ()
setAiming item = do
  RogueHarvest {..} <- getState
  let (filterMethod, noTarget) = case item of
        (Just Hoe) -> (isTillable _farm, "No tillable ground around you")
        (Just Watercan) -> (isWaterable, "No plant to water around you")
        (Just (Seed _)) -> (isPlantable _farm, "No tilled ground around you")
        Nothing -> (isHarvestable, "Nothing to harvest around you")
        _ -> (const . const $ False, "You cannot use that for anything")
      targets = filter (filterMethod _entities) $ _playerPos : getNeightbouringTiles _playerPos
  if null targets
    then addLog [(bnw, noTarget)] >> setCurrentMode (Playing Walking)
    else setCurrentMode . Playing . Aiming $ AimingMode targets item Nothing

pickSelectedTile :: (Monad m) => V2 Cell -> V2 Cell -> [V2 Cell] -> EventHandlerM m RogueHarvest RHEvents Names ()
pickSelectedTile (V2 0 0) from cells =
  case find (== from) cells of
    Nothing -> unhandled
    Just _ -> modifyState $ (currentMode . _Playing . _Aiming . currentTarget) ?~ from
pickSelectedTile dir from cells =
  -- Honestly, this is completely overkill; as Rogueharvest only let you affect things
  -- in your immediate vicinity, we don't need such a complicated way of matching
  -- direction. This is more here as a POC (to prove that implementing more powerful tools,
  -- for instance, that let you target several tiles, would be doable)
  let unitVector (V2 x y) =
        let gcdVal = gcd (abs (fromEnum x)) (abs (fromEnum y)) :: Int
         in if gcdVal == 0
              then V2 0 0
              else V2 (toEnum $ fromEnum x `div` gcdVal) (toEnum $ fromEnum y `div` gcdVal)
      dirUnit :: V2 Int
      dirUnit = unitVector dir
      matchesDirection candidate =
        let delta = candidate - from
         in delta /= V2 0 0 && unitVector delta == dirUnit
      matching = filter matchesDirection cells
   in case matching of
        [] -> unhandled
        (target : _) -> modifyState $ (currentMode . _Playing . _Aiming . currentTarget) ?~ target

-- Handle events during the watering animation
handleWateringAnimationEvents :: (Monad m) => WateringAnimationState -> EventHandler m RogueHarvest RHEvents Names
handleWateringAnimationEvents WateringAnimationState {..} RogueHarvest {..} = \case
  Step -> do
    let newStepsElapsed = _animationStepsElapsed + 1
        animationComplete = newStepsElapsed >= 10
    if animationComplete
      then do
        let (_, entities') = waterPlant _animationTarget _entities
        modifyState (\s -> s {_entities = entities'})
        setAiming (Just Watercan)
      else do
        modifyState $ (currentMode . _Playing . _WateringAnimation . animationStepsElapsed) .~ newStepsElapsed
  -- By using pure and not unhandled, we block inputs during the animation
  _ -> pure ()
