{-# LANGUAGE FlexibleContexts #-}

module Rogui.Components.TextWrapTest
  ( textWrapTests,
  )
where

import Linear (V4 (..))
import Rogui.Components.TextWrap
import Rogui.Graphics
import Test.Tasty
import Test.Tasty.HUnit

testRGBA :: RGBA
testRGBA = V4 255 255 255 255

testColours :: Colours
testColours = Colours (Just testRGBA) Nothing

redColours :: Colours
redColours = Colours (Just (V4 255 0 0 255)) Nothing

blueColours :: Colours
blueColours = Colours (Just (V4 0 0 255 255)) Nothing

-- Test splitChunk with text that fits entirely
testSplitChunkFits :: IO ()
testSplitChunkFits =
  let chunk = (testColours, "Hello world")
      (fitted, remaining) = splitChunk 20 chunk
   in do
        assertEqual "Should fit entire chunk" (Just chunk) fitted
        assertEqual "Should have empty remaining" (testColours, "") remaining

-- Test splitChunk with text that needs to wrap
testSplitChunkWraps :: IO ()
testSplitChunkWraps =
  let chunk = (testColours, "This is a long message that needs wrapping")
      (fitted, remaining) = splitChunk 20 chunk
   in do
        case fitted of
          Nothing -> assertFailure "Should fit at least some words"
          Just (_, fittedText) -> do
            assertBool "Fitted text should not be empty" (not $ null fittedText)
            assertBool "Fitted text should be shorter than limit" (length fittedText <= 20)
            assertBool "Fitted text should contain some words" (not . null . words $ fittedText)
        let (_, remainingText) = remaining
        assertBool "Should have remaining text" (not $ null remainingText)
        -- Check that original content is preserved
        case fitted of
          Just (_, fittedText) -> do
            let reconstructed = mconcat [fittedText, " ", remainingText]
            assertEqual "Content should be preserved" "This is a long message that needs wrapping" reconstructed
          Nothing -> assertFailure "Should have fitted text"

-- Test splitChunk with a single word that's too long
testSplitChunkLongWord :: IO ()
testSplitChunkLongWord =
  let longWord = "Supercalifragilisticexpialidocious"
      chunk = (testColours, longWord)
      (fitted, remaining) = splitChunk 10 chunk
   in do
        -- When a single word is too long, splitChunk returns Nothing for fitted
        assertEqual "Should not fit anything" Nothing fitted
        assertEqual "Should have original chunk as remaining" chunk remaining

-- Test splitChunk with exactly fitting text
-- Note: splitChunk accounts for spaces between words, so "Hello" (5 chars)
-- needs width of at least 6 to account for potential trailing space
testSplitChunkExactFit :: IO ()
testSplitChunkExactFit =
  let chunk = (testColours, "Hello")
      (fitted, remaining) = splitChunk 6 chunk
   in do
        assertEqual "Should fit entire chunk" (Just chunk) fitted
        assertEqual "Should have empty remaining" (testColours, "") remaining

-- Test getTextLikeUntil with multiple chunks that all fit
testGetTextLikeUntilAllFit :: IO ()
testGetTextLikeUntilAllFit =
  let chunks =
        [ (testColours, "Hello"),
          (redColours, "world"),
          (blueColours, "test")
        ]
      (fitted, remaining) = getTextLikeUntil 50 (length . snd) splitChunk chunks
   in do
        assertEqual "Should fit all chunks" chunks fitted
        assertEqual "Should have no remaining chunks" [] remaining

-- Test getTextLikeUntil with chunks that need splitting
testGetTextLikeUntilNeedsSplit :: IO ()
testGetTextLikeUntilNeedsSplit =
  let chunks =
        [ (testColours, "This is"),
          (redColours, "a long message that definitely needs wrapping"),
          (blueColours, "more text")
        ]
      (fitted, remaining) = getTextLikeUntil 30 (length . snd) splitChunk chunks
   in do
        assertBool "Should fit at least one chunk" (not $ null fitted)
        assertBool "Should have remaining chunks" (not $ null remaining)
        -- The total length of fitted text should not exceed limit significantly
        -- (accounting for spaces between chunks)
        let totalFittedLength = sum $ map (length . snd) fitted
        assertBool "Fitted text should respect width limit" (totalFittedLength <= 35)

-- Test getTextLikeUntil with empty list
testGetTextLikeUntilEmpty :: IO ()
testGetTextLikeUntilEmpty =
  let chunks = []
      (fitted, remaining) = getTextLikeUntil 20 (length . snd) splitChunk chunks
   in do
        assertEqual "Should have no fitted chunks" [] fitted
        assertEqual "Should have no remaining chunks" [] remaining

-- Test getTextLikeUntil with single chunk that needs splitting
testGetTextLikeUntilSingleChunkSplit :: IO ()
testGetTextLikeUntilSingleChunkSplit =
  let chunk = (testColours, "This is a very long message that will need to be split")
      (fitted, remaining) = getTextLikeUntil 20 (length . snd) splitChunk [chunk]
   in do
        assertBool "Should fit at least part of the chunk" (not $ null fitted)
        assertBool "Should have remaining text" (not $ null remaining)
        case (fitted, remaining) of
          ([(_fittedColour, fittedText)], [(_remainingColour, remainingText)]) -> do
            assertBool "Fitted text should not be empty" (not $ null fittedText)
            assertBool "Fitted text should be under limit" (length fittedText <= 20)
            assertBool "Remaining text should not be empty" (not $ null remainingText)
          _ -> assertFailure "Should have exactly one fitted and one remaining chunk"

-- Test behavior with multiple short chunks
testGetTextLikeUntilMultipleShort :: IO ()
testGetTextLikeUntilMultipleShort =
  let chunks =
        [ (testColours, "A"),
          (redColours, "B"),
          (blueColours, "C"),
          (testColours, "D"),
          (redColours, "E")
        ]
      (fitted, remaining) = getTextLikeUntil 10 (length . snd) splitChunk chunks
   in do
        -- All chunks should fit since they're all 1 character
        assertEqual "Should fit all short chunks" 5 (length fitted)
        assertEqual "Should have no remaining" [] remaining

textWrapTests :: TestTree
textWrapTests =
  testGroup
    "TextWrap"
    [ testCase "splitChunk - text fits entirely" testSplitChunkFits,
      testCase "splitChunk - text needs wrapping" testSplitChunkWraps,
      testCase "splitChunk - single long word" testSplitChunkLongWord,
      testCase "splitChunk - exact fit" testSplitChunkExactFit,
      testCase "getTextLikeUntil - all chunks fit" testGetTextLikeUntilAllFit,
      testCase "getTextLikeUntil - needs splitting" testGetTextLikeUntilNeedsSplit,
      testCase "getTextLikeUntil - empty list" testGetTextLikeUntilEmpty,
      testCase "getTextLikeUntil - single chunk split" testGetTextLikeUntilSingleChunkSplit,
      testCase "getTextLikeUntil - multiple short chunks" testGetTextLikeUntilMultipleShort
    ]
