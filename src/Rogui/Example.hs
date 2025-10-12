{-# LANGUAGE FlexibleContexts #-}

module Rogui.Example where

import Control.Monad.State (gets)
import Rogui.Class

data State = State
  { counter :: Int
  }

-- simpleGUI :: (GuiRogue State w String m) => m ()
-- simpleGUI = do
--   counter <- gets counter
--   withWindow "left" $ \w -> do
--     withBorder w $
--       withLines
--         w
--         [ str white black "Hello",
--           str white black ("Value is " <> show . T.pack $ counter)
--         ]
