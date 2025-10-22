{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Rogui.Application.Event
  ( Event (..),
    EventResult (..),
    MouseEventDetails (..),
    MouseMoveDetails (..),
    KeyDownDetails (..),
    KeyUpDetails (..),
    EventHandlingState (..),
    EventHandlingM,
    halt,
    redraw,
    modifyState,
    fireAppEvent,
    setCurrentState,
    fireEvent,
    popEvent,
    getExtentSize,
  )
where

import Control.Monad.State hiding (state)
import qualified Data.Map.Strict as M
import Data.Sequence (Seq (..), (|>))
import Rogui.Components.Types (Extent (..), ExtentMap)
import Rogui.Graphics.Types (Cell, Pixel)
import SDL (EventPayload, Keysym, MouseButtonEventData)
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
  | MouseClick MouseButtonEventData

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
