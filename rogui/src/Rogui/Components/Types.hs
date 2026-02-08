module Rogui.Components.Types
  ( Component (..),
    DrawingContext (..),
    DrawM,
    Extent (..),
    ExtentMap,
    Size (..),
    emptyComponent,
  )
where

import Control.Monad.State.Strict (StateT)
import Control.Monad.Writer.Strict (Writer)
import qualified Data.Map as M
import Linear (V2)
import Rogui.Graphics (Brush, Console)
import Rogui.Graphics.DSL.Instructions (Instructions)
import Rogui.Graphics.Types (Cell)

-- The actual stack used when drawing
type DrawM n a = StateT (DrawingContext n) (Writer Instructions) a

-- | Components are composable widgets (a bit like Brick, but
-- in less sophisticated).
--
-- Rather than trying to set `horizontalSize` and `verticalSize`
-- manually, you should compose with `vSized` and `hSized` for
-- simplicity's sake.
--
-- The meat of the component is `Draw`: a function that handles
-- its rendering, according to a `DisplayContext`. Rendering is
-- done by giving a series of `Instruction` from `Rogui.Graphics.DSL.Instructions`.
--
-- This datatype doesn't handle anything related to interactivity, and only concerns
-- itself with _rendering_. Interactions are dependent upon
-- the client's state. See the different components and the
-- various demo executables to get an idea of how to implement
-- interactions. The `n` parameter (for `Name`) is used specifically
-- for this, so you have a way to retrieve some information about
-- the component when handling events.
--
-- Components are written (and should be written) using their local
-- coordinates (so 0,0 is *their* top left, not the global one).
--
-- To ensure that components are properly laid out, you should wrap
-- them in a layout component, that will ensure they each receive
-- their own dedicated consoles. The main layouts available by default in
-- Rogui are the simple `vBox` and `hBox` components, or the
-- more involved `Grid` component.
data Component n = Component
  { draw :: DrawM n (),
    verticalSize :: Size,
    horizontalSize :: Size
  }

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
  deriving (Eq)

type ExtentMap n = M.Map n Extent

-- | The available state when rendering components.  The `draw` function exposed
-- by components execute with this type in its `State`. Technically, `steps`
-- should be a read only value, but to avoid a complicated monadic stack, it
-- is exposed in the state; similarly, `currentExtents` should be a Writer
-- (there is no reason to read from it when rendering). But we already use
-- a Writer for instructions, so again, extents are stored as a state for
-- simplicity sake.
--
-- When implementing your own components, you're most likely going to need
-- `console` to compute the available size when drawing and `steps` for
-- simple animations.
--
-- If you need to know the actual size and position of a component for
-- later (typically, when processing events), use `recordExtents` so that
-- the `currentExtents` map stores it.
data DrawingContext n = DrawingContext
  { -- | The current brush being used
    brush :: !Brush,
    -- | The current console being used
    console :: !Console,
    -- | The number of steps events emitted by the application. Can be
    -- used for simple animations.
    steps :: !Int,
    -- | Total elapsed time since application start, in seconds.
    totalElapsedTime :: !Double,
    -- | Time since last frame, in seconds.
    deltaTime :: !Double,
    -- | The size of named components after rendering them if `recordExtent` was used.
    currentExtents :: !(ExtentMap n)
  }

-- | A size type expressing how layout should try to size a component.  A
-- `Greedy` component should take as much size as possible. When there are
-- several Greedy components, the available space will be equitably spread
-- between them. Rogui default layout tools _don't_ take into consideration the
-- actual size of components when doing this, our system is not as smart as
-- what Brick does.
data Size = Greedy | Fixed Cell
  deriving (Eq)

emptyComponent :: Component name
emptyComponent =
  Component {draw = pure (), verticalSize = Greedy, horizontalSize = Greedy}