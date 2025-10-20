{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Rogui.Components.MessageLog
  ( messageLog,
    getTextLikeUntil,
  )
where

import Control.Monad (void)
import Control.Monad.Writer
import Data.Foldable (foldrM, toList, traverse_)
import qualified Data.Sequence as Seq
import Rogui.Components.Types (Component (..), DrawingContext (..), TileSize (..), emptyComponent)
import Rogui.Graphics (Cell (..), Instructions, setColours, str, (./.=))
import Rogui.Graphics.Console (TextAlign (TLeft))
import Rogui.Graphics.DSL.Instructions (Colours (..), newLine)
import Rogui.Graphics.Types (Console (..))

type LogChunk = (Colours, String)

type LogMessage = [LogChunk]

splitChunk :: Int -> (Colours, String) -> (Maybe (Colours, String), (Colours, String))
splitChunk n (rgb, content) =
  let splitByWord = words content
      (fittingWords, nonFittingWord) = getTextLikeUntil n (\w -> length w + 1) (\_ t -> (Nothing, t)) splitByWord
   in if not . null $ fittingWords
        then (Just (rgb, unwords fittingWords), (rgb, unwords nonFittingWord))
        else
          (Nothing, (rgb, content))

getTextLikeUntil :: Int -> (t -> Int) -> (Int -> t -> (Maybe t, t)) -> [t] -> ([t], [t])
getTextLikeUntil width getLength breaker ts =
  let folder (size, collected, left) item
        | (getLength item + size) > width =
            -- Item is too big. Let's try to break it
            case breaker (width - size) item of
              (Just enough, tooBig) -> (width, collected Seq.|> enough, left Seq.|> tooBig)
              (Nothing, tooBig) ->
                (width, collected, left Seq.|> tooBig)
        | otherwise =
            (size + getLength item, collected Seq.|> item, left)
      getResult (_, taken, left) = (toList $ taken, toList left)
   in getResult . foldl' folder (0, Seq.empty, Seq.empty) $ ts

drawMessageLog :: Cell -> LogMessage -> Cell -> Writer Instructions Cell
drawMessageLog _ _ 0 = pure 0
drawMessageLog width msg remainingLines = do
  let msgLength = Cell $ sum $ (fmap (length . snd)) msg
  if msgLength > width
    then do
      let (onLine, nextLines) = getTextLikeUntil (getCell width) (length . snd) splitChunk msg
      traverse_ drawChunk onLine
      newLine
      drawMessageLog width nextLines (remainingLines - 1)
    else do
      traverse_ drawChunk msg
      newLine
      pure (remainingLines - 1)

drawChunk :: LogChunk -> Writer Instructions ()
drawChunk (chunkColour, chunkTxt) = do
  setColours chunkColour
  str TLeft chunkTxt

truncateWordBy :: Int -> String -> String
truncateWordBy beyond w
  | length w > beyond = take (beyond - 3) w <> "..."
  | otherwise = w

truncateWordsOverWidth :: Int -> LogMessage -> LogMessage
truncateWordsOverWidth beyond msg =
  let over :: LogChunk -> LogChunk
      over = fmap (unwords . fmap (truncateWordBy beyond) . words)
   in fmap over msg

drawMessageLogs :: [LogMessage] -> DrawingContext -> Writer Instructions ()
drawMessageLogs msgs DrawingContext {tileSize = TileSize {..}, console = Console {..}} =
  let maxWidth = width ./.= pixelWidth
      availableLines = height ./.= pixelHeight
      truncated = fmap (truncateWordsOverWidth $ getCell maxWidth) msgs
   in void $ foldrM (drawMessageLog maxWidth) availableLines truncated

-- | A message log that tries to display as many messages as possible, depending
-- on the drawing context. In a game, the actual log list might be quite long;
-- you should not pass the whole list to this component, but only what would
-- typically fit.  E.g.: on a messageLog of Fixed 10 in height, just pass the 10
-- last item of your logs.
--
-- Logs are displayed in the _reverse_ order. In a typical roguelike, the log
-- would probably be a Sequence or a Vector or any type where getting the last
-- elements should not have a high complexity.
--
-- There is no guarantee everything can be displayed: long lines will be
-- wrapped, words that exceed the width will be truncated. Pay attention that
-- this implementation cuts on whitespaces when trying to wrap, so any
-- intentional double whitespace will get removed.  Finally, note that words
-- that are longer than the width will be truncated.
messageLog :: [LogMessage] -> Component n
messageLog msgs =
  emptyComponent {draw = drawMessageLogs . filter (not . null) $ msgs}