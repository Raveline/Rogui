{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}

module Rogui.Application.Event.Handlers
  ( baseEventHandler,
    keyPressHandler,
  )
where

import qualified Data.Map as M
import qualified Data.Set as S
import Rogui.Application.Event.Monad (halt, redraw, unhandled)
import Rogui.Application.Event.Types (Event (..), EventHandler, KeyDetails (..), KeyDownDetails (..), Modifier (LeftCtrl))
import qualified SDL

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
baseEventHandler ::
  -- | Sink for events that have not been processed.
  EventHandler state e n
baseEventHandler _ event =
  let ctrlC (KeyDetails SDL.KeycodeC [LeftCtrl]) = True
      ctrlC _ = False
   in case event of
        KeyDown KeyDownDetails {key} -> if ctrlC key then halt (pure ()) else unhandled
        OtherSDLEvent SDL.QuitEvent -> halt . pure $ ()
        Quit -> halt . pure $ ()
        OtherSDLEvent (SDL.WindowShownEvent _) -> redraw (pure ()) >> unhandled
        Step -> redraw (pure ()) >> unhandled
        _ -> unhandled

-- | A utility to react to key presses listed in a Map
keyPressHandler ::
  -- | A map of expected key codes and the actions to perform if this key was pressed
  M.Map (SDL.Keycode, S.Set Modifier) (EventHandler state e n) ->
  EventHandler state e n
keyPressHandler keyMap state event =
  case event of
    KeyDown KeyDownDetails {key} ->
      let handler = (keycode key, modifiers key) `M.lookup` keyMap
       in maybe unhandled (\h -> h state event) handler
    _ -> unhandled