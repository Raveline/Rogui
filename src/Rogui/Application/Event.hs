{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Even management logic and utilities. Events come in mostly three flavours:
--
--   * SDL-based events;
--
--   * Events provided by RoGUI for focus handling and animations;
--
--   * Custom events (under the `AppEvent` contructor).
--
-- Events are to be handled in the `onEvent` function of your `Rogui` object.
module Rogui.Application.Event
  ( -- * Event type definitions
    Event (..),
    EventResult (..),
    MouseEventDetails (..),
    MouseMoveDetails (..),
    MouseClickDetails (..),
    KeyDownDetails (..),
    KeyUpDetails (..),

    -- * Event handling monad
    EventHandlingState (..),
    EventHandlingM,

    -- ** Event result utilities
    halt,
    redraw,

    -- ** Event handling monad utilities
    modifyState,
    fireAppEvent,
    setCurrentState,
    fireEvent,
    foundClickedExtents,
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
  = -- | Simple adaptation of SDL key down event
    KeyDown KeyDownDetails
  | -- | Simple adaptation of SDL key up event
    KeyUp KeyUpDetails
  | -- | Simple adaption of SDL mouse events
    MouseEvent MouseEventDetails
  | -- | Store every other SDL events
    OtherSDLEvent EventPayload
  | -- | Request focus on the previous element
    FocusPrev
  | -- | Request focus on the next element
    FocusNext
  | -- | Quit signal (equivalent to SDL.Quit event)
    Quit
  | -- | Fired everytime `timerStep` amount of millisecond is reached
    Step
  | -- | Any custom event defined by the roguelike
    AppEvent e

data KeyDownDetails = KeyDownDetails
  { -- | Is this a continuous key down
    repeat :: Bool,
    key :: SDL.Keysym
  }

data KeyUpDetails = KeyUpDetails
  { key :: SDL.Keysym
  }

data MouseEventDetails
  = MouseMove MouseMoveDetails
  | MouseClick MouseClickDetails

data MouseClickDetails = MouseClickDetails
  { -- | Mouse position in pixel on the window
    absoluteMousePosition :: V2 Pixel,
    -- | Mouse position in cells on the window (computed using the default brush tilesize)
    defaultTileSizePosition :: V2 Cell,
    -- | What button was clicked (SDL constant)
    buttonCliked :: MouseButton
  }

data MouseMoveDetails = MouseMoveDetails
  { -- | Mouse move between the previous position (taken from SDL)
    relativeMouseMotion :: V2 Pixel,
    -- | Mouse position in pixel on the window
    absoluteMousePosition :: V2 Pixel,
    -- | Mouse position in cells on the window (computed using the default brush tilesize)
    defaultTileSizePosition :: V2 Cell
  }

-- | Type used to drive the behaviour of the app loop after collection events.
-- Default is `ContinueNoRedraw`. If a component (or a custom event) demands a
-- redraw, you should call `setResult Continue`. These calls get `mappended`
-- (Continue having precedence over ContinueNoRedraw).
data EventResult
  = -- | Draw everything and proceed to a new loop
    Continue
  | -- | Proceed to a new loop, without redrawing anything
    ContinueNoRedraw
  | -- | Do not loop again, quit the application
    Halt
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

-- | State available while processing events.
data EventHandlingState s e n = EventHandlingState
  { -- | Events to be processed.
    events :: Seq (Event e),
    -- | Expected behaviour in the application loop once all events have been processed.
    result :: EventResult,
    -- | Application state
    currentState :: s,
    -- | Map of all named components which record their extents at rendering
    knownExtents :: ExtentMap n
  }

-- | Convenience state modification to set the `EventResult`
setResult :: EventResult -> EventHandlingM s e n ()
setResult er =
  modify $ \ehs@EventHandlingState {result} -> ehs {result = result <> er}

-- | Convenience state modification to stop the loop (and quit)
halt :: EventHandlingM s e n () -> EventHandlingM s e n ()
halt f = setResult Halt >> f

-- | Convenience state modification to require a redraw
redraw :: EventHandlingM s e n () -> EventHandlingM s e n ()
redraw f = setResult Continue >> f

-- | Convenience modification of the Event handling state to modify the application state.
modifyState :: (state -> state) -> EventHandlingM state e n ()
modifyState f =
  modify $ \ehs@EventHandlingState {currentState} -> ehs {currentState = f currentState}

-- | Convenience modification of the Event handling state to actually _set_ the whole state
setCurrentState :: state -> EventHandlingM state e n ()
setCurrentState s =
  modify $ \ehs -> ehs {currentState = s}

-- | Convenience state modification to append a new event to handle
fireEvent :: Event e -> EventHandlingM state e n ()
fireEvent e =
  modify $ \ehs@EventHandlingState {events} -> ehs {events = events |> e}

-- | Convenience state modification wrapping a new event in `AppEvent`
fireAppEvent :: e -> EventHandlingM state e n ()
fireAppEvent e = fireEvent $ AppEvent e

-- | A simple utility mostly there to guarantee we can change
-- the monadic stack if we ever need to without having to
-- rewrite all the signatures.
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