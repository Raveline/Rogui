{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Rogui.Components.Types
  ( Component (..),
    DrawingContext (..),
    Size (..),
    DrawM,
    Extent (..),
    ExtentMap,
    -- Convenience reexport
    TileSize (..),
    emptyComponent,
    contextCellWidth,
    contextCellHeight,
    recordExtent,
    vSize,
    hSize,
    changeBrush,
    changeConsole,
    isInExtent,
  )
where

import Control.Monad.State.Strict (StateT, get, modify)
import Control.Monad.Writer.Lazy
import qualified Data.Map as M
import Rogui.Graphics (Brush, Pixel, (./.=))
import Rogui.Graphics.DSL.Instructions
import Rogui.Graphics.Types (Brush (Brush, tileHeight, tileWidth), Cell, Console (..), TileSize (..))
import SDL (V2 (..))

-- | A concept borrowed from Brick: store the actual size
-- of a component so it can be retrieved later on. This is
-- used notably for events and for viewports management.
data Extent = Extent
  { -- Actual screen position in cells
    extentPosition :: V2 Cell,
    -- Actual width/height rendered
    extentSize :: V2 Cell,
    -- The actual console used (in pixels)
    extentConsole :: Console
  }

type ExtentMap n = M.Map n Extent

data DrawingContext n = DrawingContext
  { brush :: Brush,
    console :: Console,
    steps :: Int,
    currentExtents :: ExtentMap n
  }

type DrawM n a = StateT (DrawingContext n) (Writer Instructions) a

data Size = Greedy | Fixed Cell
  deriving (Eq)

changeBrush :: Brush -> DrawM n ()
changeBrush b = do
  withBrush b
  modify $ \s -> s {brush = b}

changeConsole :: Console -> DrawM n ()
changeConsole c = do
  withConsole c
  modify $ \s -> s {console = c}

contextCellWidth :: DrawM n Cell
contextCellWidth = do
  DrawingContext {..} <- get
  let Console {width} = console
      Brush {tileWidth} = brush
  pure $ width ./.= tileWidth

contextCellHeight :: DrawM n Cell
contextCellHeight = do
  DrawingContext {..} <- get
  let Console {height} = console
      Brush {tileHeight} = brush
  pure $ height ./.= tileHeight

-- | Components are composable widgets (a bit like Brick, but
-- in less sophisticated).
-- Main field is `draw`, which receives a TileSize and a Console.
--
-- Layout components (like vBox and hBox) are responsible with handling
-- this Console object and positioning / sizing it. They also handle
-- bordering and padding computation (see vBox and hBox implementation).
--
-- Components are written (and should be written) using their local
-- coordinates (so 0,0 is *their* top left, not the global one).
-- Using border or padding will have an impact on these.
-- You can change brush during component drawing, but they are expected to
-- be the same size (given by the TileSize parameter). This is not enforced
-- though.
-- Unfortunately, some computation require reasoning in pixel width and
-- others in tile width. Both element are given as a function to ease
-- coordinates translation.
--
-- Component are parametered over a name which are used to handle focus.
data Component name = Component
  { draw :: DrawM name (),
    verticalSize :: Size,
    horizontalSize :: Size
  }

emptyComponent :: Component name
emptyComponent =
  Component {draw = pure (), verticalSize = Greedy, horizontalSize = Greedy}

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
      (V2 w h) = (V2 width height)
   in mx >= x && mx < x + w && my >= y && my < y + h