{-# LANGUAGE FlexibleContexts #-}

module Rogui.Components.Layout
  ( layoutTests,
  )
where

import Control.Monad.State.Strict (StateT (runStateT))
import Control.Monad.Writer.Strict (runWriter)
import qualified Data.Map as M
import Linear (V2 (..))
import Rogui.Components.Core
import Rogui.Graphics.Types
import Test.Tasty
import Test.Tasty.HUnit

layoutTests :: TestTree
layoutTests =
  testGroup
    "Layouts"
    [testCase "Greedy split on a hBox" testHBoxGreedySplit]

data TestComponent = LeftBox | RightBox
  deriving (Eq, Ord, Show)

testHBoxGreedySplit :: IO ()
testHBoxGreedySplit =
  let ts = TileSize 16 16
      rootConsole = Console {width = Pixel (16 * 10), height = Pixel (16 * 8), position = V2 (Pixel 0) (Pixel 0), tileSize = ts}
      -- Create two components with extent tracking
      leftComponent = withRecordedExtent LeftBox emptyComponent
      rightComponent = withRecordedExtent RightBox emptyComponent
      component = hBox [leftComponent, rightComponent]
      -- Render the layout and extract the extents
      extents = extractExtents rootConsole component
      -- Expected consoles for each half
      expectedLeftConsole = Console {width = width rootConsole `div` 2, height = height rootConsole, position = V2 (Pixel 0) (Pixel 0), tileSize = ts}
      expectedRightConsole = Console {width = width rootConsole `div` 2, height = height rootConsole, position = V2 (width rootConsole `div` 2) (Pixel 0), tileSize = ts}
   in do
        -- Check that both extents were recorded
        case (M.lookup LeftBox extents, M.lookup RightBox extents) of
          (Just leftExtent, Just rightExtent) -> do
            expectedLeftConsole @=? extentConsole leftExtent
            expectedRightConsole @=? extentConsole rightExtent
          _ -> assertFailure "Expected both LeftBox and RightBox extents to be recorded"

-- Render a component and extract the recorded extents from the DrawingContext
extractExtents :: Console -> Component TestComponent -> ExtentMap TestComponent
extractExtents root component =
  let brush = Brush {tileWidth = 16, tileHeight = 16, textureWidth = 256, textureHeight = 256, brush = undefined}
      dc = DrawingContext {brush = brush, console = root, steps = 0, currentExtents = mempty}
      (finalDC, _instructions) = runWriter (runStateT (draw component) dc)
   in currentExtents (snd finalDC)