{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Rogui.Components.ProgressBar
  ( ProgressBarDefinition (..),
    progressBar,
  )
where

import Rogui.Components.Core (Component (..), contextCellWidth, emptyComponent)
import Rogui.Graphics (Cell (..), Colours, drawHorizontalLine, movePencilBy, setColours)
import SDL (V2 (..))

-- | A way to package the numerous properties required to
-- render a progress bar
data ProgressBarDefinition = ProgressBarDefinition
  { -- | Minimum possible value
    minimumValue :: Int,
    -- | Maximum possible value
    maximumValue :: Int,
    -- | Actual current value
    value :: Int,
    -- | How the filled part should be coloured
    coloursFilled :: Colours,
    -- | How the unfilled parts should be coloured
    coloursUnfilled :: Colours,
    -- | Glyph ID to use for filled parts
    glyphFilled :: Int,
    -- | Glyph ID to use for unfilled parts
    glyphUnfilled :: Int
  }

-- | A simple (horizontal) progress bar, as used for remaining HP or MP for instance.
--
-- @
-- renderStatusBar :: Int -> Component n
-- renderStatusBar hp = hBox [
--    progressBar $ ProgressBarDefinition
--      { minimumValue = 0
--      , maximumValue = 100
--      , value = hp
--      , coloursFilled = (Colours (Just black) (Just red))
--      , coloursUnfilled = (Colours (Just black) (Just black))
--      , glyphFilled = ord ' ' -- from Data.Char
--      , glyphUnfilled = ord ' '
--      }
--   ]
-- @
progressBar :: ProgressBarDefinition -> Component n
progressBar ProgressBarDefinition {..} =
  let draw = do
        widthInTile <- contextCellWidth
        let pct = fromIntegral (value - minimumValue) / fromIntegral (maximumValue - minimumValue)
            progressed = round @Double @Cell (fromIntegral widthInTile * pct)
            remaining = widthInTile - progressed
        setColours coloursFilled
        drawHorizontalLine progressed glyphFilled
        movePencilBy (V2 progressed 0)
        setColours coloursUnfilled
        drawHorizontalLine (progressed + remaining) glyphUnfilled
   in emptyComponent {draw = draw}