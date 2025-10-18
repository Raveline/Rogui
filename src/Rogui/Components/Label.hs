{-# LANGUAGE FlexibleContexts #-}

module Rogui.Components.Label
  ( label,
  )
where

import Rogui.Components.Types (Component (..), DrawingContext (..), TileSize (..), emptyComponent)
import Rogui.Graphics.Console (TextAlign (..))
import Rogui.Graphics.DSL.Instructions (Colours, pencilAt, setColours, strLn)
import Rogui.Graphics.Types (Console (..), (./.=))
import SDL (V2 (..))

label :: String -> TextAlign -> Colours -> Component n
label content baseAlignment colours =
  let draw DrawingContext {..} = do
        let TileSize {..} = tileSize
            Console {..} = console
        setColours colours
        case baseAlignment of
          TLeft -> pure ()
          TRight -> pencilAt $ V2 ((width ./.= pixelWidth) - 1) 0
          TCenter -> pencilAt $ V2 ((width ./.= pixelWidth) `div` 2) 0
        strLn baseAlignment content
   in emptyComponent {draw = draw}