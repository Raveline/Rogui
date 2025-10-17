module Rogui.Types
  ( Rogui (..),
  )
where

import qualified Data.Map as M
import Rogui.Application.Event (Event, EventResult)
import Rogui.Components (Component)
import Rogui.Graphics.Types
import SDL (Renderer)

-- | Rogui is the main datatype used to define an application.
-- It is parametrics over:
-- - Consoles reference enum rc;
-- - Brushes reference enum rb;
-- - Components naming enum n;
-- - And finally a state.
data Rogui rc rb n state
  = Rogui
  { consoles :: M.Map rc Console,
    brushes :: M.Map rb Brush,
    rootConsole :: Console,
    defaultBrush :: Brush,
    renderer :: Renderer,
    draw :: state -> Component n,
    onEvent :: state -> Event -> (EventResult, state)
  }