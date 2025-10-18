{-# LANGUAGE FlexibleContexts #-}

module Rogui.Graphics.DSL.Instructions
  ( Instruction (..),
    Instructions,
    Colours (..),
    strLn,
    str,
    withBorder,
    withConsole,
    withBrush,
    glyphAt,
    pencilAt,
    setColours,
    drawLine,
  )
where

import Control.Monad.Writer (MonadWriter (tell))
import Data.DList
import Rogui.Graphics.Console (TextAlign (..))
import Rogui.Graphics.Primitives (RGB)
import Rogui.Graphics.Types (Brush, Console)
import SDL (V2 (..))

data Colours = Colours {front :: Maybe RGB, back :: Maybe RGB}

data Instruction
  = DrawBorder
  | DrawString TextAlign String
  | NewLine
  | OnConsole Console
  | WithBrush Brush
  | DrawGlyph Int
  | MoveTo (V2 Int)
  | MoveBy (V2 Int)
  | SetColours Colours
  | DrawLine (V2 Int)

type Instructions = DList Instruction

-- | Write the string with the provided alignment, then moves the cursor one
-- line below.
strLn :: (MonadWriter Instructions m) => TextAlign -> String -> m ()
strLn align txt =
  tell (singleton $ DrawString align txt)
    >> tell (singleton NewLine)

-- | Write the string with the alignment, then moves the cursor right
-- after the last character
str :: (MonadWriter Instructions m) => TextAlign -> String -> m ()
str align txt =
  tell (singleton $ DrawString align txt)
    >> movePencilBy (V2 (length txt) 0)

withBorder :: (MonadWriter Instructions m) => m ()
withBorder = tell . singleton $ DrawBorder

withConsole :: (MonadWriter Instructions m) => Console -> m ()
withConsole console =
  tell (singleton $ OnConsole console)
    >> pencilAt (V2 0 0)

withBrush :: (MonadWriter Instructions m) => Brush -> m ()
withBrush brush =
  tell (singleton $ WithBrush brush)

glyphAt :: (MonadWriter Instructions m) => V2 Int -> Int -> m ()
glyphAt at glyphId =
  pencilAt at
    >> tell (singleton $ DrawGlyph glyphId)

pencilAt :: (MonadWriter Instructions m) => V2 Int -> m ()
pencilAt at =
  tell (singleton $ MoveTo at)

movePencilBy :: (MonadWriter Instructions m) => V2 Int -> m ()
movePencilBy by =
  tell (singleton $ MoveBy by)

setColours :: (MonadWriter Instructions m) => Colours -> m ()
setColours colours =
  tell (singleton $ SetColours colours)

-- | A dummy drawline function. This is not an implementation of
-- Bresenham or nothing of the sort; it will simply put a space
-- character with the colour background of the pencil,
-- on each tile at a step calculated between from and to.
-- This is mostly meant to be used for horizontal highlighting.
drawLine :: (MonadWriter Instructions m) => V2 Int -> m ()
drawLine to =
  tell (singleton $ DrawLine to)