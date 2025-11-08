{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

-- | Core component system for RoGUI.
--
-- This module provides everything you need to build and compose UI components:
--
-- * Component types and rendering context
-- * Layout combinators (vBox, hBox)
-- * Visual modifiers (bordered, padded, filled)
-- * Size management utilities
module Rogui.Components.Core
  ( -- * Size management
    Size (..),
    vSize,
    hSize,

    -- * Layout components
    vBox,
    hBox,
    layered,

    -- * Visual modifiers
    bordered,
    padded,
    filled,
    switchBrush,
    trySwitchBrush,

    -- * Extent tracking
    Extent (..),
    ExtentMap,
    recordExtent,
    isInExtent,
    withRecordedExtent,

    -- * Drawing utilities
    contextCellWidth,
    contextCellHeight,
    changeBrush,
    changeConsole,

    -- * Convenience re-exports
    TileSize (..),
    DrawM,
    DrawingContext (..),
    Component (..),
    emptyComponent,
  )
where

import Control.Monad (foldM_, when)
import Control.Monad.State.Strict (get, gets, modify)
import Data.Foldable (traverse_)
import qualified Data.Map as M
import Rogui.Components.Types
import Rogui.Graphics (Brush (Brush, tileHeight, tileWidth), Colours, Pixel, RGB, setConsoleBackground, (./.=))
import Rogui.Graphics.DSL.Instructions (setColours, withBorder, withBrush, withConsole)
import Rogui.Graphics.Types (Cell (..), Console (..), TileSize (..), fromBrush, (.*=.))
import SDL (V2 (..), (^*))

changeBrush :: Brush -> DrawM n ()
changeBrush b = do
  withBrush b
  modify $ \s -> s {brush = b}

changeConsole :: Console -> DrawM n ()
changeConsole c = do
  withConsole c
  modify $ \s -> s {console = c}

-- | Utility to know the width in cell of the current console.
-- This lets you know how much cells you have to render.
contextCellWidth :: DrawM n Cell
contextCellWidth = do
  DrawingContext {..} <- get
  let Console {width} = console
      Brush {tileWidth} = brush
  pure $ width ./.= tileWidth

-- | Utility to know the height in cell of the current console.
-- This lets you know how much cells you have to render.
contextCellHeight :: DrawM n Cell
contextCellHeight = do
  DrawingContext {..} <- get
  let Console {height} = console
      Brush {tileHeight} = brush
  pure $ height ./.= tileHeight

vSize :: Size -> Component n -> Component n
vSize size component = component {verticalSize = size}

hSize :: Size -> Component n -> Component n
hSize size component = component {horizontalSize = size}

-- | Save the actual rendered size of this component, using its name to keep
-- track of it. This is useful for interactive components (focusable,
-- clickable...) or components with a viewport attached to them. Event
-- handler can then query the extent using the same name to know
-- the size and position of the component.
recordExtent :: (Ord n) => n -> DrawM n ()
recordExtent name = do
  DrawingContext {console, brush, currentExtents} <- get
  let extent = consoleToExtent brush console
  modify $ \s -> s {currentExtents = M.insert name extent currentExtents}

-- | Record the extent of any arbitrary component, under the given
-- name. Useful when you need the extent for a component that doesn't
-- typically record it.
withRecordedExtent :: (Ord n) => n -> Component n -> Component n
withRecordedExtent n child =
  let draw' = do
        recordExtent n
        draw child
   in emptyComponent {draw = draw'}

consoleToExtent :: Brush -> Console -> Extent
consoleToExtent Brush {tileWidth, tileHeight} console@Console {position, width, height} =
  let (V2 x y) = position
   in Extent
        { extentPosition = V2 (x ./.= tileWidth) (y ./.= tileHeight),
          extentSize = V2 (width ./.= tileWidth) (height ./.= tileHeight),
          extentConsole = console
        }

-- | Mostly used to check for mouse click. Given a mouse coordinate
-- in pixels, check if it is inside an extent
isInExtent :: V2 Pixel -> Extent -> Bool
isInExtent (V2 mx my) Extent {extentConsole} =
  let Console {..} = extentConsole
      (V2 x y) = position
      (V2 w h) = V2 width height
   in mx >= x && mx < x + w && my >= y && my < y + h

-- | Fill all the inner component console with the given color
filled :: RGB -> Component n -> Component n
filled rgb n =
  let draw' = do
        setConsoleBackground rgb
        draw n
   in emptyComponent {draw = draw'}

-- | Draw components aligned vertically.
--
-- Unless you specify a fixed size (see `vSize`), components will take all the
-- available space and be padded accordingly if they're too small.
--
-- Example:
--
-- @
-- verticalTexts :: Component n
-- verticalTexts =
--    vBox [ label "Hello" TLeft (Colours (Just white) (Just black))
--         , label "world" TLeft (Colours (Just white) (Just black))
--         ]
-- @
vBox :: [Component n] -> Component n
vBox components = emptyComponent {draw = layout Vertical components}

-- | Draw components aligned horizontally
--
-- Unless you specify a fixed size (see `hSize`), components will take all the
-- available space and be padded accordingly if they're too small.
--
-- Example:
--
-- @
-- horizontalTexts :: Component n
-- horizontalTexts =
--    hBox [ label "Hello" TLeft (Colours (Just white) (Just black))
--         , label "world" TLeft (Colours (Just white) (Just black))
--         ]
-- @
hBox :: [Component n] -> Component n
hBox components = emptyComponent {draw = layout Horizontal components}

-- | Safely switch to a different brush for rendering children components.
-- If the new brush tilesize doesn't match the console expected tilesize,
-- silently keeps the current brush instead.
--
-- TODO: Mismatches should be logged (logging support to be added later).
trySwitchBrush :: Brush -> Component n -> Component n
trySwitchBrush newBrush children =
  let draw' = do
        oldBrush <- gets brush
        Console {tileSize = consoleTileSize} <- gets console
        let brushTileSize = fromBrush newBrush
        -- Only switch if tile sizes match, otherwise keep current brush
        when (brushTileSize == consoleTileSize) $ changeBrush newBrush
        draw children
        changeBrush oldBrush
   in emptyComponent {draw = draw'}

-- | Unsafely switch to a different brush for rendering children components.
-- If the new brush tilesize doesn't match the console expected tilesize,
-- the validation error will be caught later when the component tree is rendered.
--
-- You might have several components that need different brushes
-- on the same console. This is typically the case when you render
-- the tilemap (and several layers on top of it, with various images
-- from different tilesets).
switchBrush :: Brush -> Component n -> Component n
switchBrush newBrush children =
  let draw' = do
        oldBrush <- gets brush
        changeBrush newBrush
        draw children
        changeBrush oldBrush
   in emptyComponent {draw = draw'}

-- | Add a border around a component, using the double border
-- using the glyphid expected to be found in a CCSID 437 tileset.
-- An automatic padding of 1 will be added.
bordered :: Colours -> Component n -> Component n
bordered colours child =
  let draw' = do
        setColours colours
        withBorder
        draw (padded 1 child)
   in emptyComponent {draw = draw'}

-- | Pad the content of the component from the inner console.
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
        draw child
   in emptyComponent {draw = draw'}

-- | Used for components who share the same DrawingContext.
-- Typical use-case is the main game area, where one wants to draw
-- a tileset, plus entities over.
layered :: [Component n] -> Component n
layered children = emptyComponent {draw = traverse_ draw children}

data Layout = Vertical | Horizontal
  deriving (Eq)

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
      greedySize = if numberGreedy > 0 then (pixelToTiles direction brush toPartition - sumFixed) `div` Cell numberGreedy else 0
      getSize child = case toScan child of
        Fixed n -> n
        Greedy -> greedySize
      render currentConsole child = do
        let newValue = getSize child
            drawingConsole =
              currentConsole
                { height = if direction == Vertical then tilesToPixel direction brush newValue else height,
                  width = if direction == Horizontal then tilesToPixel direction brush newValue else width
                }
        changeConsole drawingConsole
        draw child
        pure $ currentConsole {position = position currentConsole + (baseStep ^* tilesToPixel direction brush newValue)}
  foldM_ render root children