{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.Set as S
import Linear (V2 (..))
import Rogui.Application.Event
import Rogui.Application.System (RoguiConfig (..), bootAndPrintError)
import Rogui.Backend.SDL
import Rogui.Components.Core
import qualified Rogui.Components.ProgressBar as PB
import Rogui.Graphics
import Rogui.Types (ConsoleDrawers)

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
            defaultBrushPath = Right "terminal_10x16.png",
            defaultBrushTransparencyColour = pure black,
            drawingFunction = renderApp,
            stepMs = 100,
            eventFunction = baseEventHandler <||> handleEvent,
            consoleSpecs = [],
            brushesSpecs = [],
            allowResize = True,
            maxEventDepth = 100
          }
  bootAndPrintError
    sdlBackend
    config
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
  let withShift = S.fromList [Shift]
      withCtrl = S.fromList [Ctrl]
      keyMap =
        [ (Is KLeft mempty, changeValue (subtract 1)),
          (Is KLeft withShift, changeValue (\x -> x - 5)),
          (Is KLeft withCtrl, changeValue (\x -> x - 50)),
          (Is KRight mempty, changeValue (+ 1)),
          (Is KRight withShift, changeValue (+ 5)),
          (Is KRight withCtrl, changeValue (+ 50))
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