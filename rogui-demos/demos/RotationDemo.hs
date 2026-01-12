{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Char (ord)
import Data.Foldable (traverse_)
import Linear (V2 (..))
import Rogui.Application
import Rogui.Backend.SDL
import Rogui.Components.Core
import Rogui.Graphics
import Rogui.Types (ConsoleDrawers)

data Consoles = Root
  deriving (Show, Eq, Ord)

data Brushes = Charset
  deriving (Show, Eq, Ord)

main :: IO ()
main = do
  let config =
        RoguiConfig
          { brushTilesize = TileSize 16 16,
            appName = "RoGUI rotation demo",
            consoleCellSize = V2 10 10,
            targetFPS = 60,
            rootConsoleReference = Root,
            defaultBrushReference = Charset,
            defaultBrushPath = Right "terminal_16x16.png",
            defaultBrushTransparencyColour = pure black,
            drawingFunction = renderApp,
            stepMs = 100,
            eventFunction = baseEventHandler,
            consoleSpecs = [],
            brushesSpecs = [],
            allowResize = True,
            maxEventDepth = 100
          }
  bootAndPrintError
    sdlBackend
    config
    ()

verticalTextComponent :: String -> Colours -> Component n
verticalTextComponent s colours =
  let printChar c = glyph (ord c) [Rotate R270] >> movePencilBy (V2 0 (-1))
      draw' = do
        setColours colours
        pencilAt (V2 0 (Cell $ length s))
        traverse_ printChar s
   in emptyComponent {draw = draw'}

renderApp :: ConsoleDrawers Consoles Brushes () ()
renderApp _ _ =
  let content =
        hBox
          [ verticalTextComponent "Hello" (Colours (Just white) (Just black)),
            verticalTextComponent "world" (Colours (Just white) (Just black))
          ]
   in [(Nothing, Nothing, content)]