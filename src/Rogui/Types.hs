module Rogui.Types
  ( EventHandler,
    Rogui (..),
  )
where

import qualified Data.Map as M
import Data.Word (Word32)
import Rogui.Application.Event (Event, EventHandlingM)
import Rogui.Components.Types (Component)
import Rogui.Graphics.Types
import SDL (Renderer)

type EventHandler state e = state -> Event e -> EventHandlingM state e ()

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
    draw :: state -> Component n,
    onEvent :: EventHandler state e,
    -- | Constant evaluating the amount of milliseconds since initialisation, taken from SDL.
    lastTicks :: Word32,
    -- | Step timer constant used for basic animations, expressed in milliseconds.
    -- Every time the number of milliseconds reach this, we will fire a Step event.
    timerStep :: Word32,
    lastStep :: Word32,
    -- | Number of steps taken since the beginning of the application.
    numberOfSteps :: Int,
    -- | Internal, milliseconds per frame
    targetFrameTime :: Word32
  }