{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Rogui.Components.Core
  ( vBox,
    hBox,
    bordered,
    filled,
    renderComponents,
    switchBrush,
    -- exported for tests
    Layout (..),
    layout,
    layered,
    padded,
  )
where

import Control.Monad (foldM_)
import Control.Monad.State.Strict
import Control.Monad.Writer
import Data.Bifunctor
import Data.Foldable (traverse_)
import Rogui.Components.Types (Component (..), DrawM, DrawingContext (..), ExtentMap, Size (..), changeBrush, changeConsole, emptyComponent)
import Rogui.Graphics (Brush (Brush, tileHeight, tileWidth), Colours, RGB, setConsoleBackground)
import Rogui.Graphics.DSL.Eval (evalInstructions)
import Rogui.Graphics.DSL.Instructions (setColours, withBorder, withBrush, withConsole)
import Rogui.Graphics.Types (Cell (..), Console (..), Pixel (..), (.*=.), (./.=))
import Rogui.Types (Rogui (Rogui, defaultBrush, numberOfSteps, renderer, rootConsole))
import SDL (V2 (..), (^*))

data Layout = Vertical | Horizontal
  deriving (Eq)

-- | Fill all the display context with the given color
filled :: RGB -> Component n -> Component n
filled rgb n =
  let draw' = do
        setConsoleBackground rgb
        draw n
   in emptyComponent {draw = draw'}

-- | Draw components aligned vertically.
-- NB: an unchecked assumption is that everything is done with the same brush
-- size. Layout won't work if you change brush size.
vBox :: [Component n] -> Component n
vBox components = emptyComponent {draw = layout Vertical components}

-- | Draw components aligned horizontally
-- NB: an unchecked assumption is that everything is done with the same brush
-- size. Layout won't work if you change brush size.
hBox :: [Component n] -> Component n
hBox components = emptyComponent {draw = layout Horizontal components}

-- | You might have several components that need different brushes
-- on the same console. This is typically the case when you render
-- the tilemap (and several layers on top of it, with various images
-- from different tilesets). Compose your component with this
-- to temporarily use a different brush.
switchBrush :: Brush -> Component n -> Component n
switchBrush newBrush children =
  let draw' = do
        oldBrush <- gets brush
        changeBrush newBrush
        draw children
        changeBrush oldBrush
   in emptyComponent {draw = draw'}

bordered :: Colours -> Component n -> Component n
bordered colours child =
  let draw' = do
        setColours colours
        withBorder
        draw (padded 1 child)
   in emptyComponent {draw = draw'}

padded :: Cell -> Component n -> Component n
padded n child =
  let draw' = do
        Brush {..} <- gets brush
        console@Console {..} <- gets console
        let newConsole =
              console
                { width = width - (tileWidth .*=. (n * 2)),
                  height = height - (tileHeight .*=. (n * 2)),
                  position = position + V2 (tileWidth .*=. n) (tileHeight .*=. n)
                }
        changeConsole newConsole
        (draw child)
   in emptyComponent {draw = draw'}

-- | Used for components who share the same DrawingContext.
-- Typical use-case is the main game area, where one wants to draw
-- a tileset, plus entities over.
layered :: [Component n] -> Component n
layered children = emptyComponent {draw = traverse_ draw children}

-- | These are used to clarify units (and avoid silly bugs)
tilesToPixel :: Layout -> Brush -> Cell -> Pixel
tilesToPixel l Brush {..} t = case l of
  Horizontal -> tileWidth .*=. t
  Vertical -> tileHeight .*=. t

pixelToTiles :: Layout -> Brush -> Pixel -> Cell
pixelToTiles l Brush {..} p = case l of
  Horizontal -> p ./.= tileWidth
  Vertical -> p ./.= tileHeight

layout :: Layout -> [Component n] -> DrawM n ()
layout direction children = do
  root@Console {width, height} <- gets console
  brush <- gets brush
  let toPartition = if direction == Vertical then height else width
      baseStep = if direction == Vertical then V2 0 1 else V2 1 0
      toScan = if direction == Vertical then verticalSize else horizontalSize
      numberGreedy = length . filter ((==) Greedy . toScan) $ children
      countSize =
        \case
          Greedy -> 0
          Fixed n -> n
      sumFixed = sum . map (countSize . toScan) $ children
      greedySize = if numberGreedy > 0 then ((pixelToTiles direction brush toPartition) - sumFixed) `div` (Cell numberGreedy) else 0
      getSize child = case toScan child of
        Fixed n -> n
        Greedy -> greedySize
      render currentConsole child = do
        let newValue = getSize child
            drawingConsole =
              currentConsole
                { height = if direction == Vertical then (tilesToPixel direction brush newValue) else height,
                  width = if direction == Horizontal then (tilesToPixel direction brush newValue) else width
                }
        changeConsole drawingConsole
        draw child
        pure $ currentConsole {position = (position currentConsole) + (baseStep ^* (tilesToPixel direction brush newValue))}
  foldM_ render root children

renderComponents :: (Ord n, MonadIO m) => Rogui rc rb n s e -> Brush -> Console -> Component n -> m (ExtentMap n)
renderComponents Rogui {defaultBrush, rootConsole, numberOfSteps, renderer} usingBrush usingConsole Component {..} =
  let afterRendering =
        execStateT
          rendering
          DrawingContext {brush = usingBrush, console = usingConsole, steps = numberOfSteps, currentExtents = mempty}
      (extents, instructions) = do
        first (currentExtents) $ runWriter afterRendering
      rendering = do
        withConsole usingConsole
        withBrush usingBrush
        draw
   in evalInstructions renderer rootConsole defaultBrush instructions >> pure extents