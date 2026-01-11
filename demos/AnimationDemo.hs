{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.State.Strict (gets)
import Data.Char (ord)
import qualified Data.Sequence as Seq
import qualified Data.Vector.Strict as V
import Data.Word (Word8)
import Linear (V2 (..), V4 (..), distance)
import Rogui.Animation (AnimationSequence, animateCycle)
import Rogui.Application
import Rogui.Backend.SDL
import Rogui.Components.Core
import Rogui.Components.Game (GlyphInfo (..))
import Rogui.Components.Game.GridOverlay (gridOverlay)
import Rogui.Components.Game.GridTile (gridTile, multiLayeredGrid)
import Rogui.Graphics
import Rogui.Types (ConsoleDrawers)

data Consoles = Root
  deriving (Show, Eq, Ord)

data Brushes = Charset
  deriving (Show, Eq, Ord)

-- Dungeon and tile definitions
-- ----------------------------
-- For this example, we will use a `Vector` to store the dungeon, just to
-- illustrate that any foldable can be used.

data Tile = Wall | Floor

fromString :: String -> V.Vector Tile
fromString =
  let charToTile ' ' = Floor
      charToTile _ = Wall
   in V.fromList . fmap charToTile

dungeon :: V.Vector (V.Vector Tile)
dungeon =
  [ fromString "########################################",
    fromString "########################################",
    fromString "########################################",
    fromString "####    ################################",
    fromString "####                ####################",
    fromString "####    ########### ####################",
    fromString "################### ####################",
    fromString "################### ####################",
    fromString "#############              #############",
    fromString "#############              #############",
    fromString "#############              #############",
    fromString "#############              #############",
    fromString "#############              #############",
    fromString "#############              #############",
    fromString "#############              #############",
    fromString "#############              #############",
    fromString "########################################",
    fromString "########################################",
    fromString "########################################",
    fromString "########################################",
    fromString "########################################"
  ]

getTile :: V2 Cell -> Tile
getTile (V2 (Cell x) (Cell y)) = (dungeon V.! y) V.! x

tileToGlyphInfo :: Tile -> GlyphInfo
tileToGlyphInfo = \case
  Floor -> GlyphInfo (ord ' ') (Colours (Just black) (Just black)) []
  Wall -> GlyphInfo (ord '#') (Colours (Just lightGrey) (Just black)) []

mapSize :: V2 Cell
mapSize =
  Cell <$> V2 (length (dungeon V.! 0) - 1) (length dungeon - 1)

focal :: V2 Cell
focal =
  let (V2 w h) = mapSize
   in V2 (w `div` 2) (h `div` 2)

-- Torch logic
--------------

torchRadiusFlicker :: AnimationSequence Float
torchRadiusFlicker =
  Seq.fromList
    [ (10.0, 6), -- Bright: radius 8 cells
      (9.5, 4), -- Dimmer
      (9.8, 3), -- Medium
      (9.2, 5), -- Dim
      (9.6, 4) -- Medium-bright
    ]

torchIntensityFlicker :: AnimationSequence Word8
torchIntensityFlicker =
  Seq.fromList
    [ (240, 6), -- Almost full darkness at edge
      (220, 4), -- Slightly brighter
      (230, 3), -- Darker
      (210, 5) -- Brighter
    ]

euclideanDistance :: V2 Cell -> V2 Cell -> Float
euclideanDistance from to =
  distance (fromIntegral <$> from) (fromIntegral <$> to)

-- Compute darkness overlay alpha based on distance
-- Returns Nothing if fully lit (very close to torch)
computeDarkness ::
  Float -> -- Distance from torch
  Float -> -- Current max torch radius (from animation)
  Word8 -> -- Max darkness alpha at edge (from animation)
  Maybe RGBA
computeDarkness distance' maxRadius maxAlpha
  | distance' >= maxRadius = Just (V4 0 0 0 245) -- Beyond radius - almost dark
  | otherwise =
      let ratio = distance' / maxRadius
          gradSteps = 20
          (V4 r g b _, alpha)
            | ratio < 0.15 =
                -- Very close to torch center: bright warm glow
                let centerGlow = gradient flame orange gradSteps
                    index = min (gradSteps - 1) $ floor (ratio / 0.15 * fromIntegral gradSteps)
                    V4 r' g' b' _ = centerGlow !! index
                    -- Very low alpha = subtle tint
                    alpha' = round (30 + 50 * ratio / 0.15)
                 in (V4 r' g' b' 255, alpha')
            | ratio < 0.4 =
                -- Warm glow zone: flame (red-orange) -> amber -> yellow
                let warmGrad = gradient flame yellow gradSteps
                    index = min (gradSteps - 1) $ floor (ratio / 0.4 * fromIntegral gradSteps)
                    V4 r' g' b' _ = warmGrad !! index
                    -- Low alpha near torch (brighter)
                    alpha' = round (80 * ratio / 0.4)
                 in (V4 r' g' b' 255, alpha')
            | otherwise =
                -- Darkness zone: yellow fades to black
                let darkGrad = gradient yellow black gradSteps
                    localRatio = (ratio - 0.4) / 0.6
                    index = min (gradSteps - 1) $ floor (localRatio * fromIntegral gradSteps)
                    V4 r' g' b' _ = darkGrad !! index
                    -- High alpha far from torch (darker)
                    alpha' = round (80 + localRatio * fromIntegral (maxAlpha - 80))
                 in (V4 r' g' b' 255, alpha')
       in Just (V4 r g b alpha)

-- The main overlay function to pass to gridOverlay
torchLighting ::
  V2 Cell -> -- Torch position (e.g., player position)
  V2 Cell -> -- Cell position to compute overlay for
  DrawM n (Maybe RGBA)
torchLighting torchPos cellPos = do
  steps' <- gets steps
  let distance' = euclideanDistance torchPos cellPos
      maybeRadius = animateCycle torchRadiusFlicker steps'
      maybeIntensity = animateCycle torchIntensityFlicker steps'
  pure $ case (maybeRadius, maybeIntensity) of
    (Just radius, Just intensity) ->
      computeDarkness distance' radius intensity
    _ -> Just (V4 0 0 0 255) -- Fallback: fully dark if animation fails

-- Map and rendering
-- -----------------
-- For this example, we will use a `Vector` to store the dungeon, just to
-- illustrate that any foldable can be used.

main :: IO ()
main = do
  let config =
        RoguiConfig
          { brushTilesize = TileSize 16 16,
            appName = "RoGUI animation demo",
            consoleCellSize = V2 50 38,
            targetFPS = 60,
            rootConsoleReference = Root,
            defaultBrushReference = Charset,
            defaultBrushPath = Right "terminal_16x16.png",
            defaultBrushTransparencyColour = Just black,
            drawingFunction = renderApp,
            stepMs = 80,
            eventFunction = baseEventHandler,
            consoleSpecs = [],
            brushesSpecs = [],
            allowResize = False
          }
  bootAndPrintError
    sdlBackend
    config
    ()

renderApp :: ConsoleDrawers Consoles Brushes () ()
renderApp _ _ = [(Nothing, Nothing, hBox [renderGame])]

renderGame :: Component ()
renderGame =
  multiLayeredGrid
    mapSize
    focal
    [ gridTile getTile tileToGlyphInfo,
      gridOverlay (torchLighting focal) Nothing
    ]