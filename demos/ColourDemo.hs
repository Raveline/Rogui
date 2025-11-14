{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Foldable (traverse_)
import Linear (V2 (..))
import Rogui.Application
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
            appName = "RoGUI colour demo",
            consoleCellSize = V2 50 38,
            targetFPS = 60,
            rootConsoleReference = Root,
            defaultBrushReference = Charset,
            defaultBrushPath = "terminal_16x16.png",
            drawingFunction = renderApp,
            stepMs = 100,
            eventFunction = baseEventHandler,
            consoleSpecs = [],
            allowResize = False
          }
  bootAndPrintError
    config
    pure
    ()

renderApp :: ConsoleDrawers Consoles Brushes () ()
renderApp _ _ =
  let colours =
        vBox
          [ gradientDisplay $ gradient red orange 48,
            gradientDisplay $ gradient blue purple 48,
            gradientDisplay $ gradient yellow green 48,
            gradientDisplay $ gradient silver gold 48,
            gradientDisplay $ gradient white black 48
          ]
   in [(Nothing, Nothing, colours)]

gradientDisplay :: [RGBA] -> Component n
gradientDisplay colours =
  let renderItem col = do
        setColours (Colours (Just col) (Just col))
        str TLeft " "
      draw' = traverse_ renderItem colours
   in emptyComponent {draw = draw'}