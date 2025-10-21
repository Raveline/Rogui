{-# LANGUAGE FlexibleContexts #-}

module Rogui.Components.Label
  ( label,
  )
where

import Rogui.Components.Types (Component (..), contextCellWidth, emptyComponent)
import Rogui.Graphics.Console (TextAlign (..))
import Rogui.Graphics.DSL.Instructions (Colours, pencilAt, setColours, strLn)
import SDL (V2 (..))

label :: String -> TextAlign -> Colours -> Component n
label content baseAlignment colours =
  let draw = do
        cellWidth <- contextCellWidth
        setColours colours
        case baseAlignment of
          TLeft -> pure ()
          TRight -> pencilAt $ V2 (cellWidth - 1) 0
          TCenter -> pencilAt $ V2 (cellWidth `div` 2) 0
        strLn baseAlignment content
   in emptyComponent {draw = draw}