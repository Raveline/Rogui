{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Writer
import Data.Map.Strict qualified as M
import Rogui.Example
import Rogui.Graphics.System
import Rogui.Graphics.Types
import Rogui.Types
import SDL hiding (drawLine, textureHeight, textureWidth)

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
  -- renderingThroughInstructions roGUI
  renderingThroughComponents roGUI
  pure qPressed
