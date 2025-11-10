{-# LANGUAGE DuplicateRecordFields #-}

module Rogui.Application.Event.Types
  ( Event (..),
    EventResult (..),
    EventHandler,
    EventHandlingM,
    EventHandlerM (..),
    EventHandlingState (..),
    KeyDetails (..),
    Modifier (..),
    KeyDownDetails (..),
    MouseEventDetails (..),
    MouseClickDetails (..),
    MouseMoveDetails (..),
    ClickHandler,
    (<||>),
    liftEH,
    liftApp,
  )
where

import Control.Applicative (Alternative (..))
import Control.Monad.IO.Class
import Control.Monad.State.Strict (MonadTrans (lift), StateT)
import Data.Sequence (Seq)
import qualified Data.Set as S
import Rogui.Components.Types (ExtentMap)
import Rogui.Graphics (Cell)
import Rogui.Graphics.Types (Pixel)
import SDL (EventPayload, Keycode, MouseButton, V2)

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
  { keycode :: Keycode,
    modifiers :: S.Set Modifier
  }

data Modifier
  = Shift
  | Ctrl
  | Alt
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
    buttonClicked :: MouseButton
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
-- rewrite all the signatures. You should not be using this type.
type EventHandlingM m s e n a = StateT (EventHandlingState s e n) m a

-- | Type used to know if a handler processed the event or not.
-- This is mostly to get an alternative instance, which lets
-- users chain handlers through `<|>`.
newtype EventHandlerM m s e n a = EventHandlerM {runEventHandler :: EventHandlingM m s e n (EventHandlingResult a)}

-- | The main type used when handling events. State is provided first, events as
-- a second parameter, mostly to ease use of lambda-case.
--
-- Handlers can be composed using '<||>' (try first, fall back to second):
--
-- @
-- myHandler = baseHandler <||> customHandler
-- @
--
-- Return 'unhandled' (or 'empty') to indicate a handler didn't process an event,
-- allowing the next handler in the chain to try.
type EventHandler m state e n = state -> Event e -> EventHandlerM m state e n ()

-- | A specialisation of EventHandlingM for mouse clicks
--
-- Mouse management typically require a dedicated portion of the event handler,
-- as they escape focus.
type ClickHandler m state e n a = state -> MouseClickDetails -> EventHandlerM m state e n a

instance (Monad m) => Functor (EventHandlerM m s e n) where
  fmap f (EventHandlerM m) = EventHandlerM $ do
    unwrapped <- m
    pure $ case unwrapped of
      (Handled a) -> Handled (f a)
      Unhandled -> Unhandled

instance (Monad m) => Applicative (EventHandlerM m s e n) where
  pure a = EventHandlerM (pure $ Handled a)
  liftA2 f (EventHandlerM a) (EventHandlerM b) = EventHandlerM $ do
    a' <- a
    b' <- b
    case (a', b') of
      (Handled a'', Handled b'') -> pure . Handled $ f a'' b''
      (_, _) -> pure Unhandled

instance (Monad m) => Alternative (EventHandlerM m s e n) where
  empty = EventHandlerM (pure Unhandled)
  (EventHandlerM a) <|> (EventHandlerM b) = EventHandlerM $ do
    resA <- a
    case resA of
      Unhandled -> b
      (Handled h) -> pure (Handled h)

instance (Monad m) => Monad (EventHandlerM m s e n) where
  (EventHandlerM a) >>= f = EventHandlerM $ do
    resA <- a
    case resA of
      Unhandled -> pure Unhandled
      Handled h -> runEventHandler (f h)

instance Semigroup EventResult where
  _ <> Halt = Halt
  Halt <> _ = Halt
  Continue <> ContinueNoRedraw = Continue
  ContinueNoRedraw <> Continue = Continue
  Continue <> Continue = Continue
  _ <> _ = ContinueNoRedraw

instance Monoid EventResult where
  mempty = ContinueNoRedraw

instance (MonadIO m) => MonadIO (EventHandlerM m s e n) where
  liftIO = liftApp . liftIO

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

-- | Alternative specialized over event handler.
-- This is particularly convenient when you don't want to pass through
-- the parameter of an `EventHandler`. Compare:
--
-- @
-- verboseHandler :: EventHandler s e n
-- verboseHandler s e = baseEventHandler s e <|> otherEventsHandler s e
-- @
--
-- With:
--
-- @
-- terseHandler :: EventHandler s e n
-- terseHandler = baseEventHandler <||> otherEventHandler
-- @
(<||>) :: (Monad m) => EventHandler m s e n -> EventHandler m s e n -> EventHandler m s e n
eh <||> eh' = \s e -> eh s e <|> eh' s e

-- \| Lift an action using the EventHandlingM monad in EventHandlerM newtype.
-- You should typically not use this, as `EventHandlingM` is an internal
-- utility.
liftEH :: (Monad m) => EventHandlingM m s e n a -> EventHandlerM m s e n a
liftEH a = EventHandlerM (Handled <$> a)

-- | Lift an action from your arbitrary monadic stack into EventHandlerM.
-- E.g.:
--
-- @
-- handleSpawnEvent :: (MonadRandom m) => EventHandler m State Events Names
-- handleSpawn _ = \case
--  (AppEvent SpawnEnemy) -> do
--    enemyType <- liftApp $ uniform [Goblin, Orc]
--  _ -> unhandled
-- @
liftApp :: (Monad m) => m a -> EventHandlerM m s e n a
liftApp ma = EventHandlerM $ lift $ Handled <$> ma