{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.Map.Strict as M
import Linear (V2 (..))
import Rogui.Application
import Rogui.Application.Event.Handlers (focusRingHandler)
import Rogui.Backend.SDL
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
            defaultBrushPath = Right "terminal_10x16.png",
            defaultBrushTransparencyColour = Just black,
            drawingFunction = renderApp,
            stepMs = 100,
            consoleSpecs = [],
            brushesSpecs = [],
            allowResize = True,
            eventFunction = baseEventHandler <||> mouseHandler <||> focusHandler <||> customEventHandler
          }
  bootAndPrintError
    sdlBackend
    config
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
  -- Since the button are horizontally aligned, it makes more sense
  -- to use left and right keys for managing focus. Since users
  -- could still want to use the up and down arrow, we'll simply
  -- add to the default map.
  let movementMap =
        [ (IsNoMod KLeft, ButtonFocusPrev),
          (IsNoMod KRight, ButtonFocusNext)
        ]
          <> defaultButtonKeys
      focused =
        M.fromList
          [ (ButtonInc, handleButtonEvent' movementMap increaseEvent),
            (ButtonDec, handleButtonEvent' movementMap decreaseEvent)
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