module Rogui.Types
  ( Rogui (..),
  )
where

import qualified Data.Map as M
import Rogui.Application.Event (Event, EventResult)
import Rogui.Components (Component)
import Rogui.Graphics.Types
import SDL (Renderer)

data Rogui rc rb state
  = Rogui
  { consoles :: M.Map rc Console,
    brushes :: M.Map rb Brush,
    rootConsole :: Console,
    defaultBrush :: Brush,
    renderer :: Renderer,
    draw :: state -> Component,
    onEvent :: state -> Event -> (EventResult, state)
  }