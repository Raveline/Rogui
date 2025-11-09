{-# LANGUAGE FlexibleContexts #-}

module Rogui.Graphics.DSL.Instructions
  ( Instruction (..),
    Instructions,
    drawHorizontalLine,
    drawVerticalLine,
    drawGlyphAts,
    glyph,
    glyphAt,
    movePencilBy,
    newLine,
    overlayAt,
    pencilAt,
    setColours,
    setConsoleBackground,
    strLn,
    str,
    withBorder,
    withConsole,
    withBrush,
  )
where

import Control.Monad.Writer (MonadWriter (tell))
import Data.DList
import Rogui.Graphics.Colours (Colours)
import Rogui.Graphics.Console (TextAlign (..))
import Rogui.Graphics.Primitives (RGB, RGBA, Transformation)
import Rogui.Graphics.Types (Brush, Cell (..), Console)
import SDL (V2 (..))

-- | The set of stateful, graphical instructions that can be
-- interpreted.
data Instruction
  = -- | Draw a border at the edge of the current console
    DrawBorder
  | -- | Write a string with the provided alignment. NB: alignment will only guide where characters are added, caller is responsible for putting the cursor at the prope place.
    DrawString TextAlign String
  | -- | Move the cursor one line below and reset column to 0
    NewLine
  | -- | Use the given console
    OnConsole Console
  | -- | Use the given brush
    WithBrush Brush
  | -- | Draw a given glyph id at current pencil position, with the transformation provided
    DrawGlyph Int [Transformation]
  | -- | Mowe the drawing pencil at these exact coordinates
    MoveTo (V2 Cell)
  | -- | Move the drawing pencil by a vector
    MoveBy (V2 Cell)
  | -- | Set background and foreground colours
    SetColours Colours
  | -- | Fully colour the current console
    SetConsoleBackground RGB
  | -- | Draw the given glyph at the positions given
    DrawGlyphAts [V2 Cell] Int
  | -- | Apply an alpha overlay on the given cell
    OverlayAt (V2 Cell) RGBA

type Instructions = DList Instruction

newLine :: (MonadWriter Instructions m) => m ()
newLine = tell (singleton NewLine)

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
    >> movePencilBy (V2 (Cell $ length txt) 0)

withBorder :: (MonadWriter Instructions m) => m ()
withBorder = tell . singleton $ DrawBorder

withConsole :: (MonadWriter Instructions m) => Console -> m ()
withConsole console =
  tell (singleton $ OnConsole console)
    >> pencilAt (V2 0 0)

withBrush :: (MonadWriter Instructions m) => Brush -> m ()
withBrush brush =
  tell (singleton $ WithBrush brush)

glyph :: (MonadWriter Instructions m) => Int -> [Transformation] -> m ()
glyph glyphId trans = tell $ singleton (DrawGlyph glyphId trans)

glyphAt :: (MonadWriter Instructions m) => V2 Cell -> Int -> [Transformation] -> m ()
glyphAt at glyphId trans =
  pencilAt at
    >> glyph glyphId trans

pencilAt :: (MonadWriter Instructions m) => V2 Cell -> m ()
pencilAt at =
  tell (singleton $ MoveTo at)

movePencilBy :: (MonadWriter Instructions m) => V2 Cell -> m ()
movePencilBy by =
  tell (singleton $ MoveBy by)

setColours :: (MonadWriter Instructions m) => Colours -> m ()
setColours colours =
  tell (singleton $ SetColours colours)

-- | Draw a horizontal line from a point, to a given length,
-- using the provided character
drawHorizontalLine :: (MonadWriter Instructions m) => V2 Cell -> Cell -> Int -> m ()
drawHorizontalLine (V2 fromX fromY) to =
  let cells = [V2 x y | x <- [fromX .. fromX + to], y <- [fromY]]
   in drawGlyphAts cells

-- | Draw a vertical line from a point, to a given length,
-- using the provided character
drawVerticalLine :: (MonadWriter Instructions m) => V2 Cell -> Cell -> Int -> m ()
drawVerticalLine (V2 fromX fromY) to =
  let cells = [V2 x y | x <- [fromX], y <- [fromY .. fromY + to]]
   in drawGlyphAts cells

-- | Draw the provided glyph id in all the cells given as
-- parameter
drawGlyphAts :: (MonadWriter Instructions m) => [V2 Cell] -> Int -> m ()
drawGlyphAts cells glyphId = tell (singleton $ DrawGlyphAts cells glyphId)

setConsoleBackground :: (MonadWriter Instructions m) => RGB -> m ()
setConsoleBackground r = tell (singleton $ SetConsoleBackground r)

overlayAt :: (MonadWriter Instructions m) => V2 Cell -> RGBA -> m ()
overlayAt at colour =
  tell (singleton $ OverlayAt at colour)