{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Rogui.Components.ProgressBar
  ( progressBar,
  )
where

import Rogui.Components.Types (Component (..), DrawingContext (..), TileSize (..), emptyComponent)
import Rogui.Graphics (Cell (..), drawHorizontalLine, movePencilBy, setColours)
import Rogui.Graphics.DSL.Instructions (Colours)
import Rogui.Graphics.Types (Console (..), (./.=))
import SDL (V2 (..))

progressBar :: Int -> Int -> Int -> Colours -> Colours -> Int -> Int -> Component n
progressBar minimumValue maximumValue value filled unfilled glyphFilled glyphUnfilled =
  let draw DrawingContext {..} = do
        let Console {..} = console
            TileSize {..} = tileSize
            pct = fromIntegral (value - minimumValue) / (fromIntegral (maximumValue - minimumValue))
            widthInTile = width ./.= pixelWidth
            progressed = round @Double @Cell (fromIntegral widthInTile * pct)
            remaining = widthInTile - progressed
        setColours filled
        drawHorizontalLine progressed glyphFilled
        movePencilBy (V2 progressed 0)
        setColours unfilled
        drawHorizontalLine (progressed + remaining) glyphUnfilled
   in emptyComponent {draw = draw}