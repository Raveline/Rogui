{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Linear (V2 (..))
import Rogui.Application.Event
import Rogui.Application.System (RoguiConfig (..), bootAndPrintError)
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
            defaultBrushPath = "terminal_10x16.png",
            drawingFunction = renderApp,
            stepMs = 100,
            eventFunction = baseEventHandler <||> handleFocused <||> handleFocusChange,
            consoleSpecs = [],
            allowResize = True
          }
  bootAndPrintError
    config
    pure
    . DemoState "" ""
    $ focusRing [FirstName, LastName]

handleFocused :: (Monad m) => EventHandler m DemoState () Names
handleFocused DemoState {..} e = case focusGetCurrent ring of
  (Just FirstName) -> handleTextInputEvent e firstName (\t s -> s {firstName = t})
  (Just LastName) -> handleTextInputEvent e lastName (\t s -> s {lastName = t})
  Nothing -> unhandled

handleFocusChange :: (Monad m) => EventHandler m DemoState () Names
handleFocusChange _ = \case
  FocusNext -> modifyState $ \s -> s {ring = focusNext (ring s)}
  FocusPrev -> modifyState $ \s -> s {ring = focusPrev (ring s)}
  _ -> unhandled

renderApp :: ConsoleDrawers Consoles Brushes Names DemoState
renderApp _brushes (DemoState firstName lastName ring) =
  let bnw = Colours (Just white) (Just black)
      textInputAndLabel n key content' focused =
        vSize (Fixed 3) $
          hBox
            [ hSize (Fixed 12) $ label key TLeft bnw,
              hSize (Fixed 30) $ textInput n content' bnw focused
            ]
      fullNameLabel = label ("Your name is " <> firstName <> " " <> lastName) TLeft bnw
      content =
        bordered (Colours (Just white) (Just black))
          . vBox
          $ [ textInputAndLabel FirstName "First name" firstName (focusGetCurrent ring == Just FirstName),
              textInputAndLabel LastName "Last name" lastName (focusGetCurrent ring == Just LastName),
              fullNameLabel
            ]
   in [(Nothing, Nothing, content)]