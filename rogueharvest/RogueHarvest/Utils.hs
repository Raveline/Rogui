{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

{- HLINT ignore "Move brackets to avoid $" -}

-- | Shortcuts, common patterns, stuff that is a bit "in the way".
module RogueHarvest.Utils
  ( setCurrentMode,
    getNeightbouringTiles,
    modifyEntity,
    setTile,
    addLog,
    consumeInventoryItem,
    hasInventoryItem,
    incrementInventoryItem,
    withEnergy,
  )
where

import Data.Maybe
import Data.Sequence (Seq (Empty, (:|>)), (|>))
import Lens.Micro.Platform
import Linear.V2 (V2)
import RogueHarvest.Constants
import RogueHarvest.Types
import Rogui.Application.Event (EventHandlerM, getState, modifyState)
import Rogui.Components.MessageLog (LogMessage)
import Rogui.Graphics
import Text.Read (readMaybe)

setCurrentMode :: (Monad m) => GameMode -> EventHandlerM m RogueHarvest RHEvents Names ()
setCurrentMode newMode = modifyState (currentMode .~ newMode)

modifyEntity :: (Monad m) => (EntityMap -> EntityMap) -> EventHandlerM m RogueHarvest RHEvents Names ()
modifyEntity f = modifyState (entities %~ f)

consumeInventoryItem :: (Monad m) => Item -> EventHandlerM m RogueHarvest RHEvents Names ()
consumeInventoryItem i =
  let removeIfZero n
        | n - 1 <= 0 = Nothing
        | otherwise = Just (n - 1)
   in modifyState (inventory . at i %~ (>>= removeIfZero))

hasInventoryItem :: (Monad m) => Item -> EventHandlerM m RogueHarvest RHEvents Names Bool
hasInventoryItem i = do
  st <- getState
  let value = st ^? inventory . at i . _Just
  pure $ maybe False (> 0) value

incrementInventoryItem :: (Monad m) => Item -> EventHandlerM m RogueHarvest RHEvents Names ()
incrementInventoryItem i =
  let createIfEmpty Nothing = Just 0
      createIfEmpty (Just n) = Just (n + 1)
   in modifyState (inventory . at i %~ createIfEmpty)

setTile :: (Monad m) => V2 Cell -> Tile -> EventHandlerM m RogueHarvest RHEvents Names ()
setTile pos tile = modifyState (farm . ix pos .~ tile)

getNeightbouringTiles :: V2 Cell -> [V2 Cell]
getNeightbouringTiles from =
  [ nw + from,
    up + from,
    ne + from,
    left + from,
    right + from,
    sw + from,
    down + from,
    se + from
  ]

-- Add a new message. We concatenate similar logs (though this is done in
-- a really ugly way)
addLog :: (Monad m) => LogMessage -> EventHandlerM m RogueHarvest RHEvents Names ()
addLog s = do
  previousLogs <- _logs <$> getState
  let addNewLog = modifyState (logs %~ \ls -> ls |> s)
  case previousLogs of
    Empty -> addNewLog
    (prevLogs :|> lastLog) ->
      if isSameLog lastLog s
        then modifyState (logs .~ (prevLogs |> incrementLogCount lastLog))
        else addNewLog

incrementLogCount :: LogMessage -> LogMessage
incrementLogCount [] = []
incrementLogCount msg = case last msg of
  (col, 'x' : rest)
    | isJust (readMaybe @Int rest) -> init msg <> [(col, 'x' : (show $ fromMaybe @Int 0 (readMaybe rest) + 1))]
    | otherwise -> msg <> [(col, "x2")]
  (col, _) -> msg <> [(col, "x2")]

isSameLog :: LogMessage -> LogMessage -> Bool
isSameLog (lastLog : _) (newLog : _) = lastLog == newLog
isSameLog _ _ = False

withEnergy :: (Monad m) => Int -> EventHandlerM m RogueHarvest RHEvents Names () -> EventHandlerM m RogueHarvest RHEvents Names ()
withEnergy energyCost f = do
  RogueHarvest {..} <- getState
  if _energy > energyCost
    then f >> modifyState (energy -~ energyCost)
    else addLog [(rnb, "You are too tired to do this !")]