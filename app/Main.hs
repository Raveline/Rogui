{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Writer
import Data.Map.Strict qualified as M
import Rogui.Application.Event
import Rogui.Application.System
import Rogui.Components.Core
import Rogui.Components.Label
import Rogui.Components.List
import Rogui.Components.Types
import Rogui.Example
import Rogui.Graphics
import Rogui.Types
import SDL hiding (Event, drawLine, textureHeight, textureWidth)

data State = State
  { textValue :: String,
    listOfText :: [String],
    mousePosition :: V2 Int
  }

main :: IO ()
main = do
  boot
    (TileSize 16 16)
    "RoGUI example"
    (V2 50 38)
    guiMaker
    $ State {textValue = "test", listOfText = ["Item 1", "Item 2", "Item 3"], mousePosition = V2 0 0}

guiMaker :: (MonadIO m) => SDL.Renderer -> Console -> m (Rogui Consoles Brushes State)
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
        onEvent = baseEventHandler eventHandler
      }

eventHandler :: State -> Event -> (EventResult, State)
eventHandler state = \case
  MouseEvent (MouseMove MouseMoveDetails {..}) -> (Continue, state {mousePosition = defaultTileSizePosition})
  _ -> (ContinueNoRedraw, state)

renderApp :: State -> Component
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
          hBox
            [bordered baseColours $ padded 2 $ list listOfText id TLeft baseColours highlighted Nothing]
        ]
