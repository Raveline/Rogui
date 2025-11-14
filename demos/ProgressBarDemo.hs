{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Linear (V2 (..))
import Rogui.Application.Event
import Rogui.Application.System (RoguiConfig (..), bootAndPrintError)
import Rogui.Components.Core
import qualified Rogui.Components.ProgressBar as PB
import Rogui.Graphics
import Rogui.Types (ConsoleDrawers)
import qualified SDL

data Consoles = Root
  deriving (Show, Eq, Ord)

data Brushes = Charset
  deriving (Show, Eq, Ord)

type Names = ()

newtype DemoState = DemoState {value :: Int}

main :: IO ()
main = do
  let config =
        RoguiConfig
          { brushTilesize = TileSize 10 16,
            appName = "RoGUI progressbar demo",
            consoleCellSize = V2 80 38,
            targetFPS = 60,
            rootConsoleReference = Root,
            defaultBrushReference = Charset,
            defaultBrushPath = "terminal_10x16.png",
            drawingFunction = renderApp,
            stepMs = 100,
            eventFunction = baseEventHandler <||> handleEvent,
            consoleSpecs = [],
            allowResize = True
          }
  bootAndPrintError
    config
    pure
    $ DemoState 500

baseProgressBarDefinition :: PB.ProgressBarDefinition
baseProgressBarDefinition =
  PB.ProgressBarDefinition
    { minimumValue = 0,
      maximumValue = 1000,
      value = 500,
      coloursFilled = Colours (Just lightAmber) Nothing,
      coloursUnfilled = Colours (Just lighterAmber) Nothing,
      glyphFilled = fullBlock,
      glyphUnfilled = lightShade
    }

handleEvent :: (Monad m) => EventHandler m DemoState () Names
handleEvent =
  let keyMap =
        [ ((SDL.KeycodeLeft, []), changeValue (\x -> x - 1)),
          ((SDL.KeycodeLeft, [Shift]), changeValue (\x -> x - 5)),
          ((SDL.KeycodeLeft, [Ctrl]), changeValue (\x -> x - 50)),
          ((SDL.KeycodeRight, []), changeValue (+ 1)),
          ((SDL.KeycodeRight, [Shift]), changeValue (+ 5)),
          ((SDL.KeycodeRight, [Ctrl]), changeValue (+ 50))
        ]
   in keyPressHandler keyMap

changeValue :: (Monad m) => (Int -> Int) -> EventHandler m DemoState () Names
changeValue f (DemoState v) _ =
  let PB.ProgressBarDefinition {..} = baseProgressBarDefinition
      newValue
        | f v < minimumValue = minimumValue
        | f v > maximumValue = maximumValue
        | otherwise = f v
   in modifyState (const $ DemoState newValue)

renderApp :: ConsoleDrawers Consoles Brushes Names DemoState
renderApp _brushes (DemoState currentValue) =
  let content =
        bordered (Colours (Just white) (Just black))
          . vBox
          $ [ centered . vSize (Fixed 1) $ PB.progressBar (baseProgressBarDefinition {PB.value = currentValue})
            ]
   in [(Nothing, Nothing, content)]