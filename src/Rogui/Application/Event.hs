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
  )
where

import Control.Monad.State hiding (state)
import Data.Sequence (Seq (..), (|>))
import Rogui.Graphics.Types (Pixel, Cell)
import SDL (EventPayload, Keysym, MouseButtonEventData)
import SDL.Vect (V2)

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

data EventHandlingState s e = EventHandlingState
  { events :: Seq (Event e),
    result :: EventResult,
    currentState :: s
  }

setResult :: EventResult -> EventHandlingM s e ()
setResult er =
  modify $ \ehs@EventHandlingState {result} -> ehs {result = result <> er}

halt :: EventHandlingM s e () -> EventHandlingM s e ()
halt f = setResult Halt >> f

redraw :: EventHandlingM s e () -> EventHandlingM s e ()
redraw f = setResult Continue >> f

modifyState :: (state -> state) -> EventHandlingM state e ()
modifyState f =
  modify $ \ehs@EventHandlingState {currentState} -> ehs {currentState = f currentState}

setCurrentState :: state -> EventHandlingM state e ()
setCurrentState s =
  modify $ \ehs -> ehs {currentState = s}

fireEvent :: Event e -> EventHandlingM state e ()
fireEvent e =
  modify $ \ehs@EventHandlingState {events} -> ehs {events = events |> e}

fireAppEvent :: e -> EventHandlingM state e ()
fireAppEvent e = fireEvent $ AppEvent e

popEvent :: EventHandlingM state e (Maybe (Event e))
popEvent = do
  currentEvents <- gets events
  case currentEvents of
    Empty -> pure Nothing
    (firstEvent :<| rest) -> do
      modify $ \ehs -> ehs {events = rest}
      pure $ Just firstEvent

type EventHandlingM s e a = State (EventHandlingState s e) a