{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Rogui.Components.Types
  ( Component (..),
    DrawingContext (..),
    Size (..),
    -- Convenience reexport
    TileSize (..),
    emptyComponent,
    vSize,
    hSize,
  )
where

import Control.Monad.Writer.Lazy
import Rogui.Graphics.DSL.Instructions
import Rogui.Graphics.Types (Cell, Console, TileSize (..))

data Size = Greedy | Fixed Cell
  deriving (Eq)

data DrawingContext = DrawingContext
  { tileSize :: TileSize,
    console :: Console,
    steps :: Int
  }

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
  { draw :: DrawingContext -> Writer Instructions (),
    verticalSize :: Size,
    horizontalSize :: Size
  }

emptyComponent :: Component name
emptyComponent =
  Component {draw = \_ -> pure (), verticalSize = Greedy, horizontalSize = Greedy}

vSize :: Size -> Component n -> Component n
vSize size component = component {verticalSize = size}

hSize :: Size -> Component n -> Component n
hSize size component = component {horizontalSize = size}
