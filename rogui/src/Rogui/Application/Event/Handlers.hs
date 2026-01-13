{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Rogui.Application.Event.Handlers
  ( -- * Specific tasks handlers
    baseEventHandler,
    keyPressHandler,
    focusRingHandler,

    -- * Case analysis helpers
    withAppEvent,
    withMouseEvent,
  )
where

import Data.Foldable (find, traverse_)
import qualified Data.Map as M
import Rogui.Application.Event.Keyboard (Key (..))
import Rogui.Application.Event.Monad (halt, modifyState, redraw, unhandled)
import Rogui.Application.Event.Types (Event (..), EventHandler, EventHandlerM, FocusDestination (..), KeyDownDetails (..), KeyMatch (..), Modifier (..), MouseEventDetails, matchKey)
import Rogui.FocusRing

-- | A default event handler that will:
--
-- * React to ALT+F4, clicking the window cross, or ctrl+C to quit the
-- application
--
-- * Ensure that a first render is done on window shown
--
-- * Ensure that rendering is done when Step is reached
--
-- Other events are to be manually implemented.  Feed your own event handler to
-- this so you get an easy way to leave your applications through common
-- shortcuts.
baseEventHandler :: (Monad m) => EventHandler m state e n
baseEventHandler _ event =
  let ctrlC = matchKey (Is (KChar 'c') [Ctrl])
   in case event of
        KeyDown KeyDownDetails {key} -> if ctrlC key then halt (pure ()) else unhandled
        Quit -> halt . pure $ ()
        WindowVisible -> redraw (pure ()) >> unhandled
        Step -> redraw (pure ()) >> unhandled
        _ -> unhandled

-- | A utility to react to key presses listed.
-- Be careful, if you mix scancodes and keycodes: the first match in the foldable will
-- be retained. Ideally, you should use scancodes taken from a keybinding map
-- from your application.
-- As we might need to iterate through the whole foldable, this can become fairly
-- inefficient if you have _a lot_ of key modifiers.
keyPressHandler ::
  (Monad m, Foldable f) =>
  -- | A list of expected key codes and the actions to perform if this key was pressed
  f (KeyMatch, EventHandler m state e n) ->
  EventHandler m state e n
keyPressHandler keyMatches state event =
  case event of
    KeyDown KeyDownDetails {key} ->
      let handler = (snd <$> find ((`matchKey` key) . fst) keyMatches)
       in maybe unhandled (\h -> h state event) handler
    _ -> unhandled

-- | A handler utility to work with a focus ring. It will:
--
-- * Call a handler depending on the currently focused component
-- * Apply nextFocus and prevFocus to the ring, and update the state
-- accordingly on receiving a `FocusNext` or `FocusPrev` event.
--
-- Be careful: if you forget a focusable name in the first parameter,
-- nothing will happen when it receives focus.
focusRingHandler ::
  (Ord n, Monad m) =>
  -- | Event handlers depending upon the currently focused component
  M.Map n (EventHandler m s e n) ->
  -- | Actions to perform, if any, on component receiving focus
  M.Map n (FocusDestination -> EventHandlerM m s e n ()) ->
  -- | How to access the focus ring from the state
  (s -> FocusRing n) ->
  -- | How to update the focus ring
  (FocusRing n -> s -> s) ->
  EventHandler m s e n
focusRingHandler focusedComponentHandlers onFocusedHandlers getRing setRing s =
  let handler = focusGetCurrent (getRing s) >>= (`M.lookup` focusedComponentHandlers)
      changeFocus fn dest = do
        let newRing = fn (getRing s)
            mAction = focusGetCurrent newRing >>= (`M.lookup` onFocusedHandlers)
        traverse_ ($ dest) mAction
        redraw $ modifyState (setRing newRing)
   in \case
        (Focus FocusNext) -> changeFocus focusNext FocusNext
        (Focus FocusPrev) -> changeFocus focusPrev FocusPrev
        e -> maybe unhandled (\h -> h s e) handler

-- | Utility to define a handler that will operate only on AppEvent.
-- Any other event will be marked as `unhandled`.
withAppEvent ::
  (Ord n, Monad m) =>
  (s -> e -> EventHandlerM m s e n ()) ->
  EventHandler m s e n
withAppEvent f s = \case
  AppEvent e -> f s e
  _ -> unhandled

-- | Utility to define a handler that will operate only on MouseEvents
-- Any other event will be marked as `unhandled`.
withMouseEvent ::
  (Ord n, Monad m) =>
  (s -> MouseEventDetails -> EventHandlerM m s e n ()) ->
  EventHandler m s e n
withMouseEvent f s = \case
  MouseEvent md -> f s md
  _ -> unhandled
