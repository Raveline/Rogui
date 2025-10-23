{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Rogui.Application.Event
  ( Event (..),
    EventResult (..),
    MouseEventDetails (..),
    MouseMoveDetails (..),
    MouseClickDetails (..),
    KeyDownDetails (..),
    KeyUpDetails (..),
    EventHandlingState (..),
    EventHandlingM,
    halt,
    redraw,
    foundClickedExtents,
    modifyState,
    fireAppEvent,
    setCurrentState,
    fireEvent,
    popEvent,
    getExtentSize,
    getExtentPosition,
  )
where

import Control.Monad.State hiding (state)
import qualified Data.Map.Strict as M
import Data.Sequence (Seq (..), (|>))
import Rogui.Components.Types (Extent (..), ExtentMap, isInExtent)
import Rogui.Graphics.Types (Cell, Pixel)
import SDL (EventPayload, Keysym, MouseButton (..))
import SDL.Vect (V2 (..))

data Event e
  = KeyDown KeyDownDetails
  | KeyUp KeyUpDetails
  | MouseEvent MouseEventDetails
  | OtherSDLEvent EventPayload
  | FocusPrev
  | FocusNext
  | Quit
  | Step
  | AppEvent e

data KeyDownDetails = KeyDownDetails
  { repeat :: Bool,
    key :: SDL.Keysym
  }

data KeyUpDetails = KeyUpDetails
  { key :: SDL.Keysym
  }

data MouseEventDetails
  = MouseMove MouseMoveDetails
  | MouseClick MouseClickDetails

data MouseClickDetails = MouseClickDetails
  { absoluteMousePosition :: V2 Pixel,
    defaultTileSizePosition :: V2 Cell,
    buttonCliked :: MouseButton
  }

data MouseMoveDetails = MouseMoveDetails
  { relativeMouseMotion :: V2 Pixel,
    absoluteMousePosition :: V2 Pixel,
    defaultTileSizePosition :: V2 Cell
  }

data EventResult
  = Continue
  | ContinueNoRedraw
  | Halt
  deriving (Eq)

instance Semigroup EventResult where
  _ <> Halt = Halt
  Halt <> _ = Halt
  Continue <> ContinueNoRedraw = Continue
  ContinueNoRedraw <> Continue = Continue
  Continue <> Continue = Continue
  _ <> _ = ContinueNoRedraw

instance Monoid EventResult where
  mempty = ContinueNoRedraw

data EventHandlingState s e n = EventHandlingState
  { events :: Seq (Event e),
    result :: EventResult,
    currentState :: s,
    knownExtents :: ExtentMap n
  }

setResult :: EventResult -> EventHandlingM s e n ()
setResult er =
  modify $ \ehs@EventHandlingState {result} -> ehs {result = result <> er}

halt :: EventHandlingM s e n () -> EventHandlingM s e n ()
halt f = setResult Halt >> f

redraw :: EventHandlingM s e n () -> EventHandlingM s e n ()
redraw f = setResult Continue >> f

modifyState :: (state -> state) -> EventHandlingM state e n ()
modifyState f =
  modify $ \ehs@EventHandlingState {currentState} -> ehs {currentState = f currentState}

setCurrentState :: state -> EventHandlingM state e n ()
setCurrentState s =
  modify $ \ehs -> ehs {currentState = s}

fireEvent :: Event e -> EventHandlingM state e n ()
fireEvent e =
  modify $ \ehs@EventHandlingState {events} -> ehs {events = events |> e}

fireAppEvent :: e -> EventHandlingM state e n ()
fireAppEvent e = fireEvent $ AppEvent e

popEvent :: EventHandlingM state e n (Maybe (Event e))
popEvent = do
  currentEvents <- gets events
  case currentEvents of
    Empty -> pure Nothing
    (firstEvent :<| rest) -> do
      modify $ \ehs -> ehs {events = rest}
      pure $ Just firstEvent

type EventHandlingM s e n a = State (EventHandlingState s e n) a

-- | Get the size of an extent. Note: this will return a zero vector
-- if the extent is not known, which might happen before the first
-- frame of rendering, or if you forgot to record the extent.
getExtentSize :: (Ord n) => n -> EventHandlingM state e n (V2 Cell)
getExtentSize n = do
  result <- gets (M.lookup n . knownExtents)
  pure $ case result of
    (Just Extent {..}) -> extentSize
    Nothing -> V2 0 0

-- | Get the absolute position (in cells) of an extent. Note: this will
-- return a zero vector if the extent is not known, which might happen before the first
-- frame of rendering, or if you forgot to record the extent.
getExtentPosition :: (Ord n) => n -> EventHandlingM state e n (V2 Cell)
getExtentPosition n = do
  result <- gets (M.lookup n . knownExtents)
  pure $ case result of
    (Just Extent {..}) -> extentPosition
    Nothing -> V2 0 0

-- | This will returned all known and stored extents that have been clicked.
-- If you know that there is no overlapping extent, you can pattern-match
-- directly, but in all other cases, using `elem` over the returned value
-- might be a safer option.
foundClickedExtents :: MouseClickDetails -> EventHandlingM state e n [n]
foundClickedExtents (MouseClickDetails mousePos _ _) = do
  extents <- gets knownExtents
  pure . M.keys . M.filter (isInExtent mousePos) $ extents