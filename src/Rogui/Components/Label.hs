{-# LANGUAGE FlexibleContexts #-}

module Rogui.Components.Label
  ( label,
  )
where

import Rogui.Components.Core (contextCellWidth)
import Rogui.Components.Types (Component (..), emptyComponent)
import Rogui.Graphics (Colours, TextAlign (..))
import Rogui.Graphics.DSL.Instructions (pencilAt, setColours, strLn)
import SDL (V2 (..))

-- | The basic label component: some non interactive simple text
-- to display. This is not designed for long text, there is
-- no text wrap utility.
--
-- @
-- renderTitle :: Component n
-- renderTitle = hBox [ label "NOTICE" TCenter (Colours (Just white) (Just black)) ]
-- @
label ::
  -- | Content to display
  String ->
  -- | Alignment
  TextAlign ->
  -- | Colours to use when printing the text
  Colours ->
  Component n
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