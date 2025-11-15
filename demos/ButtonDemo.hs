{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Linear (V2 (..))
import Rogui.Application
import Rogui.Application.Event.Handlers (focusRingHandler)
import Rogui.Components
import Rogui.FocusRing
import Rogui.Graphics
import Rogui.Types (ConsoleDrawers)

data Consoles = Root
  deriving (Show, Eq, Ord)

data Brushes = Charset
  deriving (Show, Eq, Ord)

data Names = ButtonInc | ButtonDec
  deriving (Eq, Ord)

newtype CustomEvent = ChangeCounter (Int -> Int)

data State = State
  { counter :: Int,
    ring :: FocusRing Names
  }

main :: IO ()
main = do
  let config =
        RoguiConfig
          { brushTilesize = TileSize 10 16,
            appName = "RoGUI button demo",
            consoleCellSize = V2 50 38,
            targetFPS = 60,
            rootConsoleReference = Root,
            defaultBrushReference = Charset,
            defaultBrushPath = "terminal_10x16.png",
            drawingFunction = renderApp,
            stepMs = 100,
            consoleSpecs = [],
            allowResize = True,
            eventFunction = baseEventHandler <||> mouseHandler <||> focusHandler <||> customEventHandler
          }
  bootAndPrintError
    config
    pure
    $ State 0 (focusRing [ButtonInc, ButtonDec])

increaseEvent :: Event CustomEvent
increaseEvent = AppEvent $ ChangeCounter (+ 1)

decreaseEvent :: Event CustomEvent
decreaseEvent = AppEvent $ ChangeCounter (subtract 1)

mouseHandler :: (Monad m) => EventHandler m State CustomEvent Names
mouseHandler _ = \case
  (MouseEvent (MouseClickPressed mc)) -> do
    exs <- foundClickedExtents mc
    case exs of
      [ButtonInc] -> fireEvent increaseEvent
      [ButtonDec] -> fireEvent decreaseEvent
      _ -> unhandled
  _ -> unhandled

customEventHandler :: (Monad m) => EventHandler m State CustomEvent Names
customEventHandler _ = \case
  (AppEvent (ChangeCounter f)) -> modifyState $ \s -> s {counter = f (counter s)}
  _ -> unhandled

focusHandler :: (Monad m) => EventHandler m State CustomEvent Names
focusHandler =
  let focused =
        [ (ButtonInc, handleButtonEvent increaseEvent),
          (ButtonDec, handleButtonEvent decreaseEvent)
        ]
   in focusRingHandler focused mempty ring (\r s -> s {ring = r})

renderApp :: ConsoleDrawers Consoles Brushes Names State
renderApp _ State {..} =
  let bnw = Colours (Just white) (Just black)
      buttons =
        vSize (Fixed 2) $
          hBox
            [ button ButtonInc "Increase counter" TCenter bnw (invert bnw) (focusGetCurrent ring == Just ButtonInc),
              button ButtonDec "Decrease counter" TCenter bnw (invert bnw) (focusGetCurrent ring == Just ButtonDec)
            ]
      counterDisplay = label ("Counter value is : " <> show counter) TCenter bnw
      content =
        centered . vSize (Fixed 10) $
          vBox
            [ buttons,
              counterDisplay
            ]
   in [(Nothing, Nothing, content)]