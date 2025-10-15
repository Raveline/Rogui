{-# LANGUAGE ImportQualifiedPost #-}
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
import SDL hiding (drawLine, textureHeight, textureWidth)

data State = State
  { textValue :: String,
    listOfText :: [String]
  }

main :: IO ()
main = do
  boot
    (TileSize 16 16)
    "RoGUI example"
    (V2 50 38)
    guiMaker
    $ State {textValue = "test", listOfText = ["Item 1", "Item 2", "Item 3"]}

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
        onEvent = baseEventHandler (\_ s -> (ContinueNoRedraw, s))
      }

renderApp :: State -> Component
renderApp State {..} =
  let baseColours = Colours (Just white) (Just black)
      highlighted = Colours (Just black) (Just white)
   in vBox
        [ ( label
              textValue
              TCenter
              ( Colours
                  (Just red)
                  (Just black)
              )
          )
            { vSize = Fixed 3
            },
          hBox
            [bordered baseColours $ padded 2 $ list listOfText id TLeft baseColours highlighted Nothing]
        ]
