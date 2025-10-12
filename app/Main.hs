{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Writer
import Data.Map.Strict qualified as M
import Rogui.Graphics.Console
import Rogui.Graphics.Constants
import Rogui.Graphics.DSL.Eval (evalInstructions)
import Rogui.Graphics.DSL.Instructions
import Rogui.Graphics.System
import Rogui.Graphics.Types
import Rogui.Types
import SDL hiding (drawLine, textureHeight, textureWidth)

data Consoles = Root | LittleModal
  deriving (Eq, Ord)

data Brushes = Charset | Drawings
  deriving (Eq, Ord)

main :: IO ()
main = do
  boot
    (TileSize 16 16)
    "RoGUI example"
    (V2 50 38)
    guiMaker
    appLoop

guiMaker :: (MonadIO m) => SDL.Renderer -> Console -> m (Rogui Consoles Brushes)
guiMaker renderer root = do
  let modal = Console {width = 400, height = 200, position = V2 (16 * 10) (16 * 10)}
  charset <- loadBrush renderer "terminal_16x16.png" (V2 16 16)
  tiles <- loadBrush renderer "supermarket.png" (V2 16 16)
  pure $
    Rogui
      { consoles = M.fromList [(Root, root), (LittleModal, modal)],
        brushes = M.fromList [(Charset, charset), (Drawings, tiles)],
        renderer = renderer
      }

draw :: (MonadIO m) => Rogui Consoles Brushes -> m ()
draw Rogui {..} =
  evalInstructions renderer (consoles M.! Root) (brushes M.! Charset) $ execWriter $ do
    withConsole (consoles M.! Root)
    setColours (Colours (Just white) (Just black))
    withBorder
    glyphAt (V2 2 2) 65
    glyphAt (V2 3 2) 65
    pencilAt (V2 1 5)
    strLn TLeft "Hello"
    pencilAt (V2 10 6)
    strLn TRight "World"
    pencilAt (V2 10 7)
    strLn TCenter "Hello world"

    pencilAt (V2 1 20)
    setColours (Colours Nothing (Just white))
    drawLine (V2 20 20)
    drawLine (V2 1 25)
    setColours (Colours Nothing Nothing)
    withBrush (brushes M.! Drawings)
    glyphAt (V2 1 1) 0
    glyphAt (V2 2 1) 1
    glyphAt (V2 3 1) 2

appLoop :: (MonadIO m) => Rogui Consoles Brushes -> m Bool
appLoop roGUI = do
  events <- pollEvents
  let eventIsQPress event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed
              && keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False
      qPressed = any eventIsQPress events
  -- rendererDrawColor renderer $= V4 0 0 255 255
  draw roGUI
  pure qPressed
