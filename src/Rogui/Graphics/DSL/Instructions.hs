{-# LANGUAGE FlexibleContexts #-}

-- | A basic DSL to write a sequence of `Instruction` that will be
-- statefully interpreted by `Rogui.Graphis.DSL.Eval.evalInstructions`.
-- The functions defined in this module are what `Rogui.Components.Types.Component` are expected to use when defining how they are drawn. Here is
-- an example to draw a small text with changing colours:
--
-- @
-- setColours (Colour (Just yellow) (Just black))
-- strLn TLeft "It is a period of civil war. Rebel spaceship,"
-- strLn TLeft "striking from a hidden base, have won their first"
-- str TLeft "victory against the evil"
-- setColours (Colour (Just red) (Just black))
-- str TLeft "Galactic Empire"
-- setColours (Colour (Just yellow) (Just black))
-- str "."
-- @
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
    overlayConsole,
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
  | -- | Apply an overlay over the whole console
    FullConsoleOverlay RGBA

type Instructions = DList Instruction

-- | Set pencil at the leftmost cell on the line after the current one.
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

-- | Add a border on the current console. Border drawing algorithm
-- expects a CSSID 437 tileset.
withBorder :: (MonadWriter Instructions m) => m ()
withBorder = tell . singleton $ DrawBorder

-- | Use the provided console for the subsequent instructions.
withConsole :: (MonadWriter Instructions m) => Console -> m ()
withConsole console =
  tell (singleton $ OnConsole console)
    >> pencilAt (V2 0 0)

-- | Use the provided brush for the subsequent instructions.
withBrush :: (MonadWriter Instructions m) => Brush -> m ()
withBrush brush =
  tell (singleton $ WithBrush brush)

-- | Draw a glyph at the current pencil position, applying
-- the eventual transformations provided.
glyph :: (MonadWriter Instructions m) => Int -> [Transformation] -> m ()
glyph glyphId trans = tell $ singleton (DrawGlyph glyphId trans)

-- | Draw a glyph at the given position, applying the eventual transformations
-- provided.
glyphAt :: (MonadWriter Instructions m) => V2 Cell -> Int -> [Transformation] -> m ()
glyphAt at glyphId trans =
  pencilAt at
    >> glyph glyphId trans

-- | Move the pencil at the given coordinates.
pencilAt :: (MonadWriter Instructions m) => V2 Cell -> m ()
pencilAt at =
  tell (singleton $ MoveTo at)

-- | Move the pencil by the given vector.
movePencilBy :: (MonadWriter Instructions m) => V2 Cell -> m ()
movePencilBy by =
  tell (singleton $ MoveBy by)

-- | Set the current drawing colours.
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

-- | Set the current console background colour.
setConsoleBackground :: (MonadWriter Instructions m) => RGB -> m ()
setConsoleBackground r = tell (singleton $ SetConsoleBackground r)

-- | Add a RGBA overlay (with alpha blending) on the given cell.
overlayAt :: (MonadWriter Instructions m) => V2 Cell -> RGBA -> m ()
overlayAt at colour =
  tell (singleton $ OverlayAt at colour)

-- | Add a RGBA overlay over a whole console
overlayConsole :: (MonadWriter Instructions m) => RGBA -> m ()
overlayConsole colour = tell (singleton $ FullConsoleOverlay colour)