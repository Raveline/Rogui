{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Rogui.Components.ProgressBar
  ( progressBar,
  )
where

import Rogui.Components.Types (Component (..), contextCellWidth, emptyComponent)
import Rogui.Graphics (Cell (..), Colours, drawHorizontalLine, movePencilBy, setColours)
import SDL (V2 (..))

progressBar :: Int -> Int -> Int -> Colours -> Colours -> Int -> Int -> Component n
progressBar minimumValue maximumValue value filled unfilled glyphFilled glyphUnfilled =
  let draw = do
        widthInTile <- contextCellWidth
        let pct = fromIntegral (value - minimumValue) / (fromIntegral (maximumValue - minimumValue))
            progressed = round @Double @Cell (fromIntegral widthInTile * pct)
            remaining = widthInTile - progressed
        setColours filled
        drawHorizontalLine progressed glyphFilled
        movePencilBy (V2 progressed 0)
        setColours unfilled
        drawHorizontalLine (progressed + remaining) glyphUnfilled
   in emptyComponent {draw = draw}