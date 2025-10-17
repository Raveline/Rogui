{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Rogui.Components.Types
  ( Component (..),
    Size (..),
    -- Convenience reexport
    TileSize (..),
    emptyComponent,
  )
where

import Control.Monad.Writer.Lazy
import Rogui.Graphics.DSL.Instructions
import Rogui.Graphics.Types (Console, TileSize (..))

data Size = Greedy | Fixed Int
  deriving (Eq)

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
  { draw :: TileSize -> Console -> Writer Instructions (),
    vSize :: Size,
    hSize :: Size
  }

emptyComponent :: Component name
emptyComponent =
  Component {draw = \_ _ -> pure (), vSize = Greedy, hSize = Greedy}