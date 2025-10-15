{-# LANGUAGE FlexibleContexts #-}

module Rogui.Components.Label
  ( label,
  )
where

import Rogui.Components.Types (Component (..), TileSize (..), emptyComponent)
import Rogui.Graphics.Console (TextAlign (..))
import Rogui.Graphics.DSL.Instructions (Colours, pencilAt, setColours, strLn)
import Rogui.Graphics.Types (Console (..))
import SDL (V2 (..))

label :: String -> TextAlign -> Colours -> Component
label content baseAlignment colours =
  let draw TileSize {..} Console {..} = do
        setColours colours
        case baseAlignment of
          TLeft -> pure ()
          TRight -> pencilAt $ V2 ((width `div` pixelWidth) - 1) 0
          TCenter -> pencilAt $ V2 ((width `div` pixelWidth) `div` 2) 0
        strLn baseAlignment content
   in emptyComponent {draw = draw}