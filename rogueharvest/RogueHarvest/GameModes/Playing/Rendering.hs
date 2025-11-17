{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module RogueHarvest.GameModes.Playing.Rendering
  ( renderGrid,
    renderSleepDialog,
  )
where

import Data.Array
import Data.Char
import qualified Data.Map.Strict as M
import Data.Maybe
import Lens.Micro.Platform
import Linear (V2 (V2), V4 (..))
import RogueHarvest.Components.ConfirmDialog
import RogueHarvest.Constants
import RogueHarvest.Types
import Rogui.Components
import Rogui.Components.Game
import Rogui.Graphics

-- This is the main utility to display the game grid. We use
-- rogui's multiLayeredGrid that is going to provide us a viewport
-- on which we can paint all our layers:
--
-- - First, tiles;
-- - Then, entities (in our demo, these are only plants);
-- - Then, the water effect for watered plants;
-- - Finally, a last layer handle some UI overlay like the currently targetable
--   or targeted tiles; it's also where player gets drawn, as it should always be
--   on top of everything.

renderGrid :: M.Map Brushes Brush -> RogueHarvest -> Component Names
renderGrid _ RogueHarvest {..} =
  multiLayeredGrid
    (snd $ bounds _farm)
    _playerPos
    [ gridTile (\pos -> (pos, _farm ! pos)) (uncurry tileToGlyphInfo),
      renderEntities _entities,
      renderWateredLayer _currentMode _entities,
      gameModeLayer _currentMode _playerPos
    ]

-- To render watered crops, we simply add a blue-ish lightshade glyph on top of
-- the previously renderer plant. It's not very realistic (watered ground doesn't
-- look blue) but we opted for a more symbolic approach.
-- During watering animation:
-- - Alpha fades from 255 (fully opaque) to 64 (quite transparent), creating a dramatic effect
-- - Glyph changes: fullBlock (0-4 steps) → mediumShade (5-9 steps) → lightShade (final)
renderWateredLayer :: GameMode -> EntityMap -> MapViewport -> Component Names
renderWateredLayer gm entitiesMap =
  let isWatered cc = fromMaybe False $ cc ^? cellPlant . _Just . watered
      wateredCells = fmap fst . filter (isWatered . snd) . assocs $ entitiesMap
      -- Check if we're currently animating a watering action
      (animatingCell, animationAlpha, animationGlyph) = case gm of
        Playing (WateringAnimation WateringAnimationState {..}) ->
          -- Calculate alpha: 255 at step 0, fading to 64 at step 10
          -- Linear interpolation: alpha = 255 - (stepsElapsed * 191) / 10
          -- This gives: 255 → 236 → 217 → 198 → 179 → 160 → 141 → 122 → 103 → 84 → 64
          let alpha = fromIntegral $ max 64 (255 - ((_animationStepsElapsed * 191) `div` 10))
              -- Glyph progression: fullBlock (0-4) → mediumShade (5-9) → lightShade (10+)
              glyphChar = if _animationStepsElapsed < 5
                        then fullBlock
                        else if _animationStepsElapsed < 10
                               then mediumShade
                               else lightShade
           in (Just _animationTarget, alpha, glyphChar)
        _ -> (Nothing, 128, lightShade)
      -- Render function that uses animated or static alpha and glyph
      renderWater pos =
        let (alpha, glyphChar) = if Just pos == animatingCell
                               then (animationAlpha, animationGlyph)
                               else (128, lightShade)
         in GlyphInfo glyphChar (Colours (Just (setAlpha lightBlue alpha)) Nothing) []
      -- For animation, include the animating cell even if not yet marked as watered
      cellsToRender = case animatingCell of
        Just cell | cell `notElem` wateredCells -> cell : wateredCells
        _ -> wateredCells
   in entitiesLayer cellsToRender renderWater id

-- Utility to distinguish when we're drawing player and when we are drawing
-- targets.
data GameModeEntities = PlayerAt | TargetX

-- | Display stuff that are related to the game mode.
-- In walking mode, it's only the player.
-- In aiming mode, it's the player, the targettable tiles plus the targeted one.
gameModeLayer :: GameMode -> V2 Cell -> MapViewport -> Component Names
gameModeLayer gm playerAt =
  let entFromMode (Playing (Aiming AimingMode {..})) = (TargetX,) <$> _potentialCells
      entFromMode _ = []
      entities' = (PlayerAt, playerAt) : entFromMode gm
      coloursTargettable = Colours (Just yellow) Nothing
      coloursTargetted = Colours (Just white) Nothing
      renderEntity (PlayerAt, _) = GlyphInfo 1 bnw []
      renderEntity (TargetX, pos) =
        if Just pos == (gm ^? _Playing . _Aiming . currentTarget . _Just)
          then GlyphInfo (ord 'O') coloursTargetted []
          else GlyphInfo (ord 'X') coloursTargettable []
   in entitiesLayer entities' renderEntity snd

-- Render all the entities. It's actually always plants, since these are
-- the only entities we're really supporting in this demo. But if we want
-- to add other lentities later, we can use this same layer to do so.
renderEntities :: EntityMap -> MapViewport -> Component Names
renderEntities em =
  let posAndEntities = mapMaybe (traverse _cellPlant) $ assocs em
      renderPlant PlantState {..} = renderCrop _plantCrop _growthStage
   in entitiesLayer posAndEntities (renderPlant . snd) fst

-- When user goes to their home, a dialog appears to ask if they
-- are done for the day and want to go to sleep.
renderSleepDialog :: V2 Cell -> ConfirmDialogState -> Component Names
renderSleepDialog playerPos' dialogState =
  let normalColours = Colours (Just white) (Just black)
      highlightColours = Colours (Just black) (Just white)
      -- Position the dialog slightly offset from the player
      dialogPos = playerPos' + V2 4 0
   in overlaid (V4 0 0 0 128) Nothing $
        atPosition dialogPos $
          confirmDialog
            SleepYesButton
            SleepNoButton
            "Go to sleep?"
            normalColours
            highlightColours
            dialogState