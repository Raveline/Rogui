{-# LANGUAGE FlexibleContexts #-}

module Rogui.Components.MessageLogTest
  ( messageLogTests,
  )
where

import Control.Monad.State.Strict (StateT (runStateT))
import Control.Monad.Writer.Strict (execWriter)
import qualified Data.DList as D
import Data.List
import Data.Maybe (mapMaybe)
import Linear (V2 (..), V4 (..))
import Rogui.Components.Core (Component (..), DrawingContext (..))
import Rogui.Components.MessageLog (messageLog)
import Rogui.Graphics (Colours (..))
import Rogui.Graphics.DSL.Instructions (Instruction (..))
import Rogui.Graphics.Primitives (RGBA)
import Rogui.Graphics.Types
import Test.Tasty
import Test.Tasty.HUnit

testRGBA :: RGBA
testRGBA = V4 255 255 255 255

testColours :: Colours
testColours = Colours (Just testRGBA) Nothing

extractStrings :: [Instruction] -> [String]
extractStrings = mapMaybe extractString
  where
    extractString (DrawString _ str) = Just str
    extractString _ = Nothing

countNewlines :: [Instruction] -> Int
countNewlines = length . filter isNewLine
  where
    isNewLine NewLine = True
    isNewLine _ = False

renderMessageLog :: [[LogChunk]] -> Int -> Int -> [Instruction]
renderMessageLog messages widthInCells heightInCells =
  let console =
        Console
          { width = Pixel (widthInCells * 16),
            height = Pixel (heightInCells * 16),
            position = V2 (Pixel 0) (Pixel 0),
            tileSize = TileSize 16 16
          }
      brush' = Brush {tileWidth = 16, tileHeight = 16, textureWidth = 256, textureHeight = 256, brush = undefined}
      dc :: DrawingContext Int
      dc = DrawingContext {brush = brush', console = console, steps = 0, currentExtents = mempty}
      component = messageLog messages
      instructions = execWriter . runStateT (draw component) $ dc
   in D.toList instructions

type LogChunk = (Colours, String)

testSimpleLine :: IO ()
testSimpleLine =
  let messages = [[(testColours, "Hello world")]]
      instructions = renderMessageLog messages 20 10
      strings = extractStrings instructions
      newlines = countNewlines instructions
   in do
        assertEqual "Should render the text" ["Hello world"] strings
        assertEqual "Should have one newline" 1 newlines

testWrappedLine :: IO ()
testWrappedLine =
  let messages = [[(testColours, "This is a long message that needs to be wrapped")]]
      instructions = renderMessageLog messages 20 10
      strings = extractStrings instructions
      newlines = countNewlines instructions
   in do
        assertBool "Should render at least 2 parts" (length strings >= 2)
        assertBool "Should have at least 2 newlines" (newlines >= 2)
        let rejoined = unwords strings
        assertEqual "Content should be preserved" "This is a long message that needs to be wrapped" rejoined

testSuperlongWord :: IO ()
testSuperlongWord =
  let _superlong = "Supercalifragilisticexpialidocious"
      _messages = [[(testColours, _superlong)]]
      _instructions = renderMessageLog _messages 20 10
      _strings = extractStrings _instructions
   in do
        assertBool "Should handle overlong words gracefully" True
        assertBool "Should truncate long word" (any (isSuffixOf "...") _strings)

testMultipleMessages :: IO ()
testMultipleMessages =
  let messages =
        [ [(testColours, "First message")],
          [(testColours, "Second message")],
          [(testColours, "Third message")]
        ]
      instructions = renderMessageLog messages 20 10
      strings = extractStrings instructions
      newlines = countNewlines instructions
   in do
        -- foldrM renders right-to-left, so newest (last) message appears first
        assertEqual "Should render all messages (newest first)" ["Third message", "Second message", "First message"] strings
        assertEqual "Should have three newlines" 3 newlines

testMultiColorChunks :: IO ()
testMultiColorChunks =
  let red = V4 255 0 0 255
      blue = V4 0 0 255 255
      redColours = Colours (Just red) Nothing
      blueColours = Colours (Just blue) Nothing
      messages = [[(redColours, "Red text"), (blueColours, "blue text")]]
      instructions = renderMessageLog messages 20 10
      strings = extractStrings instructions
   in do
        assertEqual "Should render both chunks with space between" ["Red text", " ", "blue text"] strings

testEmptyMessage :: IO ()
testEmptyMessage =
  let messages = [[]]
      instructions = renderMessageLog messages 20 10
      strings = extractStrings instructions
   in do
        -- Empty messages are filtered out
        assertEqual "Should render nothing for empty messages" [] strings

messageLogTests :: TestTree
messageLogTests =
  testGroup
    "MessageLog"
    [ testCase "Simple line that fits" testSimpleLine,
      testCase "Long line that needs wrapping" testWrappedLine,
      testCase "Superlong word" testSuperlongWord,
      testCase "Multiple messages" testMultipleMessages,
      testCase "Multi-color chunks" testMultiColorChunks,
      testCase "Empty message filtered" testEmptyMessage
    ]
