{-# LANGUAGE DuplicateRecordFields #-}

-- | Event management logic and utilities. Events come in mostly three flavours:
--
--   * SDL-based events;
--
--   * Events provided by RoGUI for focus handling and animations;
--
--   * Custom events (under the `AppEvent` constructor).
--
-- Events are to be handled in the `onEvent` function of your `Rogui` object.
-- See `Rogui.Types` for detailed examples on this.
--
-- = Rogui's event system primer
--
-- Event inner processing can be a bit daunting at first, so here's a small summary
-- of the main points.
--
-- == Event processing
--
-- The game loop processes SDL events first. Your handler (and the component default
-- event handler offered) can then fire more events. The loop will keep processing
-- events until there are none, or, to prevent an infinite loop, after 30 iterations
-- of the event handler in the same frame (if you need to fire 30 successive events,
-- there might be something wrong with your logic to be honest).
--
-- Events are not asynchronous, and they are handled as they come.
--
-- == EventResult vs. EventHandlingResult
--
-- These names can be a bit confusing (TODO: rename the first one).
--
-- `EventResult` is used by the main loop to know if:
--   * UI should be redrawn;
--   * The game should stop;
--
-- `EventHandlingResult` is used to allow composition of handlers through
-- an `Alternative` instance.  Event handlers are expected to behave following
-- a chain of responsability pattern. A event handler can always return
-- `unhandled` to indicate that other handlers should handle it.
--
-- Handler composition can use `<|>` if you pass all parameters, or if you don't
-- need to pass them, you can use our own operator `<||>` directly (see `<||>`
-- documentation and `Rogui.Types` for examples).
--
-- == The special case of mouse events
--
-- The typical way to handle the UI in Rogui is to rely heavily upon keyboard
-- inputs, using `FocusRing` to distribute handlers. But it is not doable
-- anymore when dealing with mouse events, since mouse disregard focus.
--
-- In practice, it means that:
--
-- * Everything that uses the mouse should be named
-- and record their extent (see `Rogui.Components.Types` and
-- `Rogui.Components.Core` on extent mechanism). Use `recordExtent` to
-- ensure that the extent of a component is stored at rendering so
-- it can be known when handling events. Note that there is always
-- one frame delay when computing an extent, but unless you're running
-- at very low FPS, it should have no impact on your users.
-- * They should have a dedicated handler. This is represented by the existence of
-- the `ClickHandler` type.
--
-- The canonical way to handle mouse click is through `foundClickedExtents`,
-- which takes a `MouseClickDetails` and return a list of names. A list is
-- provided because several components might overlap. The list is not ordered
-- in any fashion, so you have to do the hard work of thinking about z-order
-- and priorising components.
--
-- For simpler cases, a typical mouse click handler could look like this:
--
-- @
-- data Names = BigRedButton | BigBlueButton
-- data Events = Boom | NoBoom
--
-- handleClickEvent :: ClickHandler YourState YourCustomEvents YourNames ()
-- handleClickEvent state clickInfo = do
--   clicked <- foundClickedExtents clickInfo
--   when (BigRedButton `elem` clicked) $ fireCustomEvent Boom
--   when (BigBlueButton `elem` clicked) $ fireCustomEvent NoBoom
-- @
module Rogui.Application.Event
  ( -- * Event type definitions
    Event (..),
    EventResult (..),
    FocusDestination (..),
    MouseEventDetails (..),
    MouseMoveDetails (..),
    MouseClickDetails (..),
    KeyDownDetails (..),
    KeyDetails (..),
    Modifier (..),

    -- * Event handling monad
    EventHandlingState (..),
    EventHandlingM,
    ClickHandler,

    -- * Key matching utilities
    isSC,
    isSC',
    isKC,
    isKC',
    KeyMatch (..),
    KeyDetailsMatch (..),
    matchKey,
    matchKeyDetails,

    -- * Convenience default handlers
    baseEventHandler,
    keyPressHandler,

    -- ** Event result utilities
    halt,
    redraw,

    -- ** Event handling monad utilities
    modifyState,
    getState,
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

import Rogui.Application.Event.Handlers
import Rogui.Application.Event.Monad
import Rogui.Application.Event.Types