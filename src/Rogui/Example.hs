{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Rogui.Example where

import Control.Monad.Writer
import Data.Map.Strict qualified as M
import Rogui.Components (Component (..), Size (..), bordered, hBox, padded, renderComponents, vBox)
import Rogui.Components.Label (label)
import Rogui.Components.List (list, mkListState)
import Rogui.Graphics
import Rogui.Types
import SDL hiding (drawLine, textureHeight, textureWidth)

data Consoles = Root | LittleModal
  deriving (Eq, Ord)

data Brushes = Charset | Drawings
  deriving (Eq, Ord)

-- This is not the intended usage, but it's a way to give an example
-- on how the Graphics instruction layer work.
renderingThroughInstructions :: (MonadIO m) => Rogui Consoles Brushes n s () -> m ()
renderingThroughInstructions Rogui {..} =
  evalInstructions renderer (consoles M.! Root) (brushes M.! Charset) $ execWriter $ do
    withConsole (consoles M.! Root)
    setColours (Colours (Just white) (Just black))
    withBorder
    glyphAt (V2 2 2) 65
    glyphAt (V2 3 2) 65
    pencilAt (V2 1 5)
    strLn TLeft "Hello"
    pencilAt (V2 10 6)
    strLn TRight "World"
    pencilAt (V2 10 7)
    strLn TCenter "Hello world"

    pencilAt (V2 1 20)
    setColours (Colours Nothing (Just white))
    drawLine (V2 20 20)
    drawLine (V2 1 25)
    setColours (Colours Nothing Nothing)
    withBrush (brushes M.! Drawings)
    glyphAt (V2 1 1) 0
    glyphAt (V2 2 1) 1
    glyphAt (V2 3 1) 2

-- Again, this is a rather constraining way of using the library,
-- but it gives an example on how to use the Component layer.
renderingThroughComponents :: (MonadIO m) => Rogui Consoles Brushes n s () -> m ()
renderingThroughComponents Rogui {..} =
  let baseColours = Colours (Just white) (Just black)
      highlighted = Colours (Just black) (Just white)
      components =
        vBox
          [ ( label
                "Components demo"
                TCenter
                ( Colours
                    (Just red)
                    (Just black)
                )
            )
              { vSize = Fixed 3
              },
            hBox
              [ bordered baseColours $ padded 2 $ list ["Apple", "Banana", "Orange"] id TLeft baseColours highlighted mkListState,
                bordered baseColours $ list ["Amethyst", "Beryl", "Onyx"] id TLeft baseColours highlighted mkListState
              ]
          ]
   in renderComponents renderer (consoles M.! Root) (brushes M.! Charset) components