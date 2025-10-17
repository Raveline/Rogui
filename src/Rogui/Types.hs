module Rogui.Types
  ( EventHandler,
    Rogui (..),
  )
where

import qualified Data.Map as M
import Rogui.Application.Event (Event, EventHandlingM)
import Rogui.Components (Component)
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
    onEvent :: EventHandler state e
  }