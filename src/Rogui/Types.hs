module Rogui.Types
  ( Rogui (..),
  )
where

import qualified Data.Map as M
import Rogui.Graphics.Types
import SDL (Renderer)

data Rogui rc rb
  = Rogui
  { consoles :: M.Map rc Console,
    brushes :: M.Map rb Brush,
    renderer :: Renderer
  }