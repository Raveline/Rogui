{-# LANGUAGE DeriveFunctor #-}
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
    KeyDetails (..),
    Modifier (..),

    -- * Event handling monad
    EventHandlingState (..),
    EventHandlingM,
    EventHandlingResult (..),

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
    liftEH,
    unhandled,
    EventHandlerM (..),
    EventHandler,
    (<||>),
  )
where

import Control.Applicative
import Control.Monad.State hiding (state)
import qualified Data.Map.Strict as M
import Data.Sequence (Seq (..), (|>))
import qualified Data.Set as S
import Rogui.Components.Types (Extent (..), ExtentMap, isInExtent)
import Rogui.Graphics.Types (Cell, Pixel)
import SDL (EventPayload, Keycode, MouseButton (..))
import SDL.Vect (V2 (..))

data Event e
  = -- | Simple adaptation of SDL key down event
    KeyDown KeyDownDetails
  | -- | Simple adaptation of SDL key up event
    KeyUp KeyDetails
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

data KeyDetails = KeyDetails
  { keycode :: SDL.Keycode,
    modifiers :: S.Set Modifier
  }

data Modifier
  = LeftShift
  | RightShift
  | LeftCtrl
  | RightCtrl
  | LeftAlt
  | RightAlt
  | AltGr
  deriving (Eq, Ord)

data KeyDownDetails = KeyDownDetails
  { -- | Is this a continuous key down
    repeat :: Bool,
    key :: KeyDetails
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

data EventHandlingResult a = Handled a | Unhandled

-- | A simple utility mostly there to guarantee we can change
-- the monadic stack if we ever need to without having to
-- rewrite all the signatures.
type EventHandlingM s e n a = State (EventHandlingState s e n) a

-- | Type used to know if a handler processed the event or not.
-- This is mostly to get an alternative instance, which lets
-- users chain handlers through `<|>`.
newtype EventHandlerM s e n a = EventHandlerM {runEventHandler :: EventHandlingM s e n (EventHandlingResult a)}

-- | The main type used when handling events. State is provided first, events as
-- a second parameter, mostly to ease lambda-case.
-- | Event handler monad that supports composition via 'Alternative'.
--
-- Handlers can be composed using '<||>' (try first, fall back to second):
--
-- @
-- myHandler = baseHandler \<|> customHandler
-- @
--
-- Return 'unhandled' (or 'empty') to indicate a handler didn't process an event,
-- allowing the next handler in the chain to try.
type EventHandler state e n = state -> Event e -> EventHandlerM state e n ()

instance Functor (EventHandlerM s e n) where
  fmap f (EventHandlerM m) = EventHandlerM $ do
    unwrapped <- m
    pure $ case unwrapped of
      (Handled a) -> Handled (f a)
      Unhandled -> Unhandled

instance Applicative (EventHandlerM s e n) where
  pure a = EventHandlerM (pure $ Handled a)
  liftA2 f (EventHandlerM a) (EventHandlerM b) = EventHandlerM $ do
    a' <- a
    b' <- b
    case (a', b') of
      (Handled a'', Handled b'') -> pure . Handled $ f a'' b''
      (_, _) -> pure Unhandled

instance Alternative (EventHandlerM s e n) where
  empty = EventHandlerM (pure Unhandled)
  (EventHandlerM a) <|> (EventHandlerM b) = EventHandlerM $ do
    resA <- a
    case resA of
      Unhandled -> b
      (Handled h) -> pure (Handled h)

instance Monad (EventHandlerM s e n) where
  (EventHandlerM a) >>= f = EventHandlerM $ do
    resA <- a
    case resA of
      Unhandled -> pure Unhandled
      Handled h -> runEventHandler (f h)

liftEH :: EventHandlingM s e n a -> EventHandlerM s e n a
liftEH a = EventHandlerM (Handled <$> a)

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
setResult :: EventResult -> EventHandlerM s e n ()
setResult er =
  liftEH (modify $ \ehs@EventHandlingState {result} -> ehs {result = result <> er})

-- | Convenience state modification to stop the loop (and quit)
halt :: EventHandlerM s e n () -> EventHandlerM s e n ()
halt f = setResult Halt >> f

-- | Convenience state modification to require a redraw
redraw :: EventHandlerM s e n () -> EventHandlerM s e n ()
redraw f = setResult Continue >> f

-- | Convenience modification of the Event handling state to modify the application state.
modifyState :: (state -> state) -> EventHandlerM state e n ()
modifyState f =
  liftEH . modify $ \ehs@EventHandlingState {currentState} -> ehs {currentState = f currentState}

-- | Convenience modification of the Event handling state to actually _set_ the whole state
setCurrentState :: state -> EventHandlerM state e n ()
setCurrentState s =
  liftEH . modify $ \ehs -> ehs {currentState = s}

-- | Convenience state modification to append a new event to handle
fireEvent :: Event e -> EventHandlerM state e n ()
fireEvent e =
  liftEH . modify $ \ehs@EventHandlingState {events} -> ehs {events = events |> e}

-- | Convenience state modification wrapping a new event in `AppEvent`
fireAppEvent :: e -> EventHandlerM state e n ()
fireAppEvent e = fireEvent $ AppEvent e

-- | Get the size of an extent. Note: this will return a zero vector
-- if the extent is not known, which might happen before the first
-- frame of rendering, or if you forgot to record the extent.
getExtentSize :: (Ord n) => n -> EventHandlerM state e n (V2 Cell)
getExtentSize n = do
  result <- liftEH $ gets (M.lookup n . knownExtents)
  pure $ case result of
    (Just Extent {..}) -> extentSize
    Nothing -> V2 0 0

-- | Get the absolute position (in cells) of an extent. Note: this will
-- return a zero vector if the extent is not known, which might happen before the first
-- frame of rendering, or if you forgot to record the extent.
getExtentPosition :: (Ord n) => n -> EventHandlerM state e n (V2 Cell)
getExtentPosition n = do
  result <- liftEH $ gets (M.lookup n . knownExtents)
  pure $ case result of
    (Just Extent {..}) -> extentPosition
    Nothing -> V2 0 0

-- | This will returned all known and stored extents that have been clicked.
-- If you know that there is no overlapping extent, you can pattern-match
-- directly, but in all other cases, using `elem` over the returned value
-- might be a safer option.
foundClickedExtents :: MouseClickDetails -> EventHandlerM state e n [n]
foundClickedExtents (MouseClickDetails mousePos _ _) = do
  extents <- liftEH $ gets knownExtents
  pure . M.keys . M.filter (isInExtent mousePos) $ extents

-- | Shortcut to say that a event handler didn't react to an event.
-- This allow chaining handler through `<|>`.
unhandled :: EventHandlerM state e n a
unhandled = empty

-- | Alternative specialized over event handler
(<||>) :: EventHandler s e n -> EventHandler s e n -> EventHandler s e n
eh <||> eh' = \s e -> (eh s e) <|> (eh' s e)
