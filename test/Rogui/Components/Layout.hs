{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Rogui.Components.Layout
  ( layoutTests,
  )
where

import Control.Monad.Writer (execWriter)
import qualified Data.DList as D
import Data.Maybe (mapMaybe)
import Linear (V2 (..))
import Rogui.Components.Core (Layout (Horizontal), layout)
import Rogui.Components.Types (Component, DrawingContext (..), emptyComponent)
import Rogui.Graphics.DSL.Instructions (Instruction (..))
import Rogui.Graphics.Types
import Test.Tasty
import Test.Tasty.HUnit

layoutTests :: TestTree
layoutTests =
  testGroup
    "Layouts"
    [testCase "Greedy split on a hBox" testHBoxGreedySplit]

testHBoxGreedySplit :: IO ()
testHBoxGreedySplit =
  let rootConsole = Console {width = Pixel (16 * 10), height = Pixel (16 * 8), position = V2 (Pixel 0) (Pixel 0)}
      halfConsole = Console {width = width rootConsole `div` 2, height = height rootConsole, position = V2 (Pixel 0) (Pixel 0)}
      components = [emptyComponent, emptyComponent]
      consoles = extractConsoles Horizontal rootConsole components
   in [halfConsole, halfConsole {position = V2 (width rootConsole `div` 2) (Pixel 0)}] @=? consoles

-- Render a layout, get the Writer result, and filter to only get the
-- setConsole instructions; this will give the Console in their order
-- of creation.
extractConsoles :: Layout -> Console -> [Component n] -> [Console]
extractConsoles layout' root components =
  let tiles = TileSize (Pixel 16) (Pixel 16)
      dc = DrawingContext {tileSize = tiles, console = root, steps = 0}
      instructions = D.toList $ execWriter (layout layout' components dc)
      onlyConsoles = \case
        (OnConsole c) -> Just c
        _ -> Nothing
   in mapMaybe onlyConsoles instructions