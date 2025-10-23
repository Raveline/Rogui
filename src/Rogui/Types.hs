module Rogui.Types
  ( EventHandler,
    Rogui (..),
    ClickHandler,
    ConsoleDrawers,
    ToDraw,
  )
where

import qualified Data.Map as M
import Data.Word (Word32)
import Rogui.Application.Event (Event, EventHandlingM, MouseClickDetails)
import Rogui.Components.Types (Component, ExtentMap)
import Rogui.Graphics.Types
import SDL (Renderer)

type ClickHandler state e n a = state -> MouseClickDetails -> EventHandlingM state e n a

type EventHandler state e n = state -> Event e -> EventHandlingM state e n ()

type ConsoleDrawers rc rb n state = M.Map rb Brush -> state -> ToDraw rc rb n

type ToDraw rc rb n = [(Maybe rc, Maybe rb, Component n)]

-- | Rogui is the main datatype used to define an application.
-- It is parametrics over:
-- - Consoles reference enum rc;
-- - Brushes reference enum rb;
-- - Components naming enum n;
-- - A state;
-- - A custom event type e;
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
    extentsMap :: ExtentMap n
  }