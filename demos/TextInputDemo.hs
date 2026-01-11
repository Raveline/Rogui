{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Char
import Linear (V2 (..))
import Rogui.Application.Event
import Rogui.Application.Event.Handlers (focusRingHandler)
import Rogui.Application.System (RoguiConfig (..), bootAndPrintError)
import Rogui.Backend.SDL (sdlBackend)
import Rogui.Components (label)
import Rogui.Components.Core
import Rogui.Components.TextInput
import Rogui.FocusRing
import Rogui.Graphics
import Rogui.Types (ConsoleDrawers)

data Consoles = Root
  deriving (Show, Eq, Ord)

data Brushes = Charset
  deriving (Show, Eq, Ord)

data Names = FirstName | LastName
  deriving (Eq, Ord)

data DemoState = DemoState
  { firstName :: String,
    lastName :: String,
    ring :: FocusRing Names
  }

main :: IO ()
main = do
  let config =
        RoguiConfig
          { brushTilesize = TileSize 10 16,
            appName = "RoGUI TextInput demo",
            consoleCellSize = V2 80 38,
            targetFPS = 60,
            rootConsoleReference = Root,
            defaultBrushReference = Charset,
            defaultBrushPath = Right "terminal_10x16.png",
            defaultBrushTransparencyColour = pure black,
            drawingFunction = renderApp,
            stepMs = 100,
            eventFunction = baseEventHandler <||> handleFocus,
            consoleSpecs = [],
            brushesSpecs = [],
            allowResize = True
          }
  bootAndPrintError
    sdlBackend
    config
    . DemoState "" ""
    $ focusRing [FirstName, LastName]

handleFocus :: (Monad m) => EventHandler m DemoState () Names
handleFocus =
  let focus =
        [ (FirstName, \s -> handleFilteredTextInputEvent isAscii (firstName s) (\t s' -> s' {firstName = t}) s),
          (LastName, \s -> handleFilteredTextInputEvent isAscii (lastName s) (\t s' -> s' {lastName = t}) s)
        ]
   in focusRingHandler focus mempty ring (\r s -> s {ring = r})

renderApp :: ConsoleDrawers Consoles Brushes Names DemoState
renderApp _brushes (DemoState firstName' lastName' ring') =
  let bnw = Colours (Just white) (Just black)
      textInputAndLabel n key' content' focused =
        vSize (Fixed 3) $
          hBox
            [ hSize (Fixed 12) $ label key' TLeft bnw,
              hSize (Fixed 30) $ textInput n content' bnw focused
            ]
      fullNameLabel = label ("Your name is " <> firstName' <> " " <> lastName') TLeft bnw
      content =
        bordered (Colours (Just white) (Just black))
          . vBox
          $ [ textInputAndLabel FirstName "First name" firstName' (focusGetCurrent ring' == Just FirstName),
              textInputAndLabel LastName "Last name" lastName' (focusGetCurrent ring' == Just LastName),
              fullNameLabel
            ]
   in [(Nothing, Nothing, content)]