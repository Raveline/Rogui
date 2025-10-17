{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Writer
import Data.Map.Strict qualified as M
import Rogui.Application.Event
import Rogui.Application.System
import Rogui.Components.Button (button)
import Rogui.Components.Core
import Rogui.Components.Label
import Rogui.Components.List
import Rogui.Components.Types
import Rogui.Example
import Rogui.FocusRing
import Rogui.Graphics
import Rogui.Types
import SDL hiding (Event, drawLine, textureHeight, textureWidth)

data State = State
  { textValue :: String,
    listOfText :: [String],
    mousePosition :: V2 Int,
    ring :: FocusRing Name,
    listState :: ListState
  }

data Name
  = List
  | QuitButton
  deriving (Eq)

main :: IO ()
main = do
  boot
    (TileSize 16 16)
    "RoGUI example"
    (V2 50 38)
    guiMaker
    $ State
      { textValue = "test",
        listOfText = ["Item 1", "Item 2", "Item 3"],
        mousePosition = V2 0 0,
        ring = focusRing [List, QuitButton],
        listState = mkListState
      }

guiMaker :: (MonadIO m) => SDL.Renderer -> Console -> m (Rogui Consoles Brushes Name State)
guiMaker renderer root = do
  let modal = Console {width = 400, height = 200, position = V2 (16 * 10) (16 * 10)}
  charset <- loadBrush renderer "terminal_16x16.png" (V2 16 16)
  tiles <- loadBrush renderer "supermarket.png" (V2 16 16)
  pure $
    Rogui
      { consoles = M.fromList [(Root, root), (LittleModal, modal)],
        brushes = M.fromList [(Charset, charset), (Drawings, tiles)],
        rootConsole = root,
        defaultBrush = charset,
        draw = renderApp,
        renderer = renderer,
        onEvent = baseEventHandler (keyPressHandler eventHandler keysHandler)
      }

keysHandler :: M.Map SDL.Keycode (State -> (EventResult, State))
keysHandler =
  M.fromList $
    [(SDL.KeycodeTab, handleTab)]

handleTab :: State -> (EventResult, State)
handleTab s =
  let newRing = focusNext (ring s)
      -- When focus moves to List and selection is Nothing, initialize
      newListState = case (focusGetCurrent newRing, listState s) of
        (Just List, ListState Nothing offset) -> ListState (Just 0) offset
        _ -> listState s
   in (Continue, s {ring = newRing, listState = newListState})

eventHandler :: State -> Event -> (EventResult, State)
eventHandler state@State {..} = \case
  MouseEvent (MouseMove MouseMoveDetails {..}) -> (Continue, state {mousePosition = defaultTileSizePosition})
  e -> case focusGetCurrent ring of
    (Just List) -> (Continue, state {listState = handleListEvent (length listOfText) e listState})
    _ -> (ContinueNoRedraw, state)

renderApp :: State -> Component Name
renderApp State {..} =
  let baseColours = Colours (Just white) (Just black)
      highlighted = Colours (Just black) (Just white)
      (V2 x y) = mousePosition
   in vBox
        [ ( label
              ("Mouse at: " <> show (x, y))
              TRight
              baseColours
          )
            { vSize = Fixed 1
            },
          ( label
              textValue
              TCenter
              ( Colours
                  (Just red)
                  (Just black)
              )
          )
            { vSize = Fixed 2
            },
          bordered baseColours $ padded 2 $ list listOfText id TLeft baseColours highlighted listState,
          button "Quit" TCenter baseColours highlighted (focusGetCurrent ring == Just QuitButton)
        ]
