{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Rogui.Components.Layout
  ( layoutTests,
  )
where

import Control.Monad.State.Strict (StateT (runStateT))
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
extractConsoles :: Layout -> Console -> [Component Int] -> [Console]
extractConsoles layout' root components =
  let brush = Brush {tileWidth = 16, tileHeight = 16, textureWidth = 256, textureHeight = 256, brush = undefined}
      dc = DrawingContext {brush = brush, console = root, steps = 0, currentExtents = mempty}
      instructions = D.toList . execWriter . runStateT (layout layout' components) $ dc
      onlyConsoles = \case
        (OnConsole c) -> Just c
        _ -> Nothing
   in mapMaybe onlyConsoles instructions