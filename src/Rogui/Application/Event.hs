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
    setCurrentState,
    fireEvent,
    popEvent,
  )
where

import Control.Monad.State hiding (state)
import Data.Sequence (Seq (..), (|>))
import SDL (EventPayload, Keysym, MouseButtonEventData)
import SDL.Vect (V2)

data Event
  = KeyDown KeyDownDetails
  | KeyUp KeyUpDetails
  | MouseEvent MouseEventDetails
  | OtherSDLEvent EventPayload
  | FocusPrev
  | FocusNext
  | Quit

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
  { relativeMouseMotion :: V2 Int,
    absoluteMousePosition :: V2 Int,
    defaultTileSizePosition :: V2 Int
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

data EventHandlingState s = EventHandlingState
  { events :: Seq Event,
    result :: EventResult,
    currentState :: s
  }

setResult :: EventResult -> EventHandlingM s ()
setResult er =
  modify $ \ehs@EventHandlingState {result} -> ehs {result = result <> er}

halt :: EventHandlingM s () -> EventHandlingM s ()
halt f = setResult Halt >> f

redraw :: EventHandlingM s () -> EventHandlingM s ()
redraw f = setResult Continue >> f

modifyState :: (state -> state) -> EventHandlingM state ()
modifyState f =
  modify $ \ehs@EventHandlingState {currentState} -> ehs {currentState = f currentState}

setCurrentState :: state -> EventHandlingM state ()
setCurrentState s =
  modify $ \ehs -> ehs {currentState = s}

fireEvent :: Event -> EventHandlingM state ()
fireEvent e =
  modify $ \ehs@EventHandlingState {events} -> ehs {events = events |> e}

popEvent :: EventHandlingM state (Maybe Event)
popEvent = do
  currentEvents <- gets events
  case currentEvents of
    Empty -> pure Nothing
    (firstEvent :<| rest) -> do
      modify $ \ehs -> ehs {events = rest}
      pure $ Just firstEvent

type EventHandlingM s a = State (EventHandlingState s) a