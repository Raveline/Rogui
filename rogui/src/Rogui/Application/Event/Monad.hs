{-# LANGUAGE NamedFieldPuns #-}

module Rogui.Application.Event.Monad
  ( setResult,
    halt,
    redraw,
    getState,
    modifyState,
    modifyStateM,
    setCurrentState,
    fireEvent,
    fireAppEvent,
    getExtentSize,
    getExtentPosition,
    foundClickedExtents,
    unhandled,
  )
where

import Control.Applicative (Alternative (..))
import Control.Monad.State.Strict (MonadState (..), gets, modify)
import qualified Data.Map as M
import Data.Sequence ((|>))
import Linear (V2 (..))
import Rogui.Application.Event.Types
import Rogui.Components.Core (isInExtent)
import Rogui.Components.Types (Extent (..))
import Rogui.Graphics.Types (Cell)

-- | Convenience state modification to set the `EventResult`
setResult :: (Monad m) => EventResult -> EventHandlerM m s e n ()
setResult er =
  liftEH (modify $ \ehs@EventHandlingState {result} -> ehs {result = result <> er})

-- | Convenience state modification to stop the loop (and quit)
halt :: (Monad m) => EventHandlerM m s e n () -> EventHandlerM m s e n ()
halt f = setResult Halt >> f

-- | Convenience state modification to require a redraw
redraw :: (Monad m) => EventHandlerM m s e n () -> EventHandlerM m s e n ()
redraw f = setResult Continue >> f

-- | Convenience state access when handling events
getState :: (Monad m) => EventHandlerM m state e n state
getState = liftEH $ gets currentState

-- | Convenience modification of the Event handling state to modify the application state.
modifyState :: (Monad m) => (state -> state) -> EventHandlerM m state e n ()
modifyState f =
  liftEH . modify $ \ehs@EventHandlingState {currentState} -> ehs {currentState = f currentState}

-- | Convenience modification of the Event handling state to modify the application state.
modifyStateM :: (Monad m) => (state -> m state) -> EventHandlerM m state e n ()
modifyStateM f = do
  ehs@EventHandlingState {currentState} <- liftEH get
  newState <- liftApp $ f currentState
  liftEH $ put $ ehs {currentState = newState}

-- | Convenience modification of the Event handling state to actually _set_ the whole state
setCurrentState :: (Monad m) => state -> EventHandlerM m state e n ()
setCurrentState s =
  liftEH . modify $ \ehs -> ehs {currentState = s}

-- | Convenience state modification to append a new event to handle
fireEvent :: (Monad m) => Event e -> EventHandlerM m state e n ()
fireEvent e =
  liftEH . modify $ \ehs@EventHandlingState {events} -> ehs {events = events |> e}

-- | Convenience state modification wrapping a new event in `AppEvent`
fireAppEvent :: (Monad m) => e -> EventHandlerM m state e n ()
fireAppEvent e = fireEvent $ AppEvent e

-- | (Monad m) => Get the size of an extent. Note: this will return a zero vector
-- if the extent is not known, which might happen before the first
-- frame of rendering, or if you forgot to record the extent.
getExtentSize :: (Monad m, Ord n) => n -> EventHandlerM m state e n (V2 Cell)
getExtentSize n = do
  result <- liftEH $ gets (M.lookup n . knownExtents)
  pure $ case result of
    (Just Extent {..}) -> extentSize
    Nothing -> V2 0 0

-- | Get the absolute position (in cells) of an extent. Note: this will
-- return a zero vector if the extent is not known, which might happen before the first
-- frame (Monad m) => of rendering, or if you forgot to record the extent.
getExtentPosition :: (Monad m, Ord n) => n -> EventHandlerM m state e n (V2 Cell)
getExtentPosition n = do
  result <- liftEH $ gets (M.lookup n . knownExtents)
  pure $ case result of
    (Just Extent {..}) -> extentPosition
    Nothing -> V2 0 0

-- | This will returned all known and stored extents that have been clicked.
-- If you know that there is no overlapping extent, you can pattern-match
-- directly, but in all other cases, using `elem` over the returned value
-- might be a safer option.
foundClickedExtents :: (Monad m) => MouseClickDetails -> EventHandlerM m state e n [n]
foundClickedExtents (MouseClickDetails mousePos _ _) = do
  extents <- liftEH $ gets knownExtents
  pure . M.keys . M.filter (isInExtent mousePos) $ extents

-- | Shortcut to say that a event handler didn't react to an event.
-- This allow chaining handler through `<|>`.
unhandled :: (Monad m) => EventHandlerM m state e n a
unhandled = empty