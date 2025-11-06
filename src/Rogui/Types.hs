module Rogui.Types
  ( -- * Main application type
    Rogui (..),

    -- * Event handling
    EventHandler,
    ClickHandler,
    ConsoleDrawers,

    -- * Drawing
    ToDraw,
  )
where

import Data.Sequence (Seq)
import qualified Data.Map as M
import Data.Word (Word32)
import Rogui.Application.Event (EventHandler, EventHandlerM, MouseClickDetails)
import Rogui.Components.Types (Component, ExtentMap)
import Rogui.Graphics.Types
import SDL (Renderer)

-- | A specialisation of EventHandlingM for mouse clicks
--
-- Mouse management typically require a dedicated portion of the event handler,
-- as they escape focus.
type ClickHandler state e n a = state -> MouseClickDetails -> EventHandlerM state e n a

-- | Drawing function type. Drawing functions have accessed to all registered
-- brush (but these must be fetched manually when describing the expected UI).
type ConsoleDrawers rc rb n state = M.Map rb Brush -> state -> ToDraw rc rb n

-- | Expected result of the drawing function. A list of triplets containing:
--
-- * A console to use (`rootConsole` if nothing was provided);
-- * A brush to use (`defaultBrush` if nothing was provided);
-- * A component tree.
--
-- Be careful: if the brush you use doesn't match the expected tilesize in
-- the console, an exception will be thrown.
type ToDraw rc rb n = [(Maybe rc, Maybe rb, Component n)]

-- | Rogui is the main datatype used to define an application.
-- It is parametrics over:
--
-- * Consoles reference enum `rc`;
-- * Brushes reference enum `rb`;
-- * Components naming enum `n`;
-- * A `state`;
-- * A custom event type `e`.
--
-- Clients are not expected to build this manually. `boot` will handle it for
-- the users, and will fill (and handle) all the internal fields.
data Rogui rc rb n state e
  = Rogui
  { consoles :: M.Map rc Console,
    brushes :: M.Map rb Brush,
    rootConsole :: Console,
    defaultBrush :: Brush,
    renderer :: Renderer,
    draw :: ConsoleDrawers rc rb n state,
    onEvent :: EventHandler state e n,
    -- | Constant evaluating the amount of milliseconds since initialisation, taken from SDL.
    lastTicks :: Word32,
    -- | Step timer constant used for basic animations, expressed in milliseconds.
    -- Every time the number of milliseconds reach this, we will fire a Step event.
    timerStep :: Word32,
    lastStep :: Word32,
    -- | Number of steps taken since the beginning of the application.
    numberOfSteps :: Int,
    -- | Internal, milliseconds per frame
    targetFrameTime :: Word32,
    -- | List of known extents
    extentsMap :: ExtentMap n,
    -- | Rolling window of recent frame durations (in milliseconds) for FPS tracking
    recentFrameTimes :: Seq Word32,
    -- | Timestamp of last FPS warning (for rate limiting)
    lastFPSWarning :: Word32
  }