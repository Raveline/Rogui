module Rogui.Components
  ( module Rogui.Components.Core,
    module Rogui.Components.Button,
    module Rogui.Components.Grid,
    module Rogui.Components.Label,
    module Rogui.Components.MessageLog,
    module Rogui.Components.MultilineText,
    module Rogui.Components.ProgressBar,
    module Rogui.Components.TextInput,
    module Rogui.Components.Types,
  )
where

-- \| == Components primer
--
-- Rogui separates the rendering in two main layers: a low level set of instructions
-- that are defined in `Graphics.DSL.Instructions`, and a higher level view, which
-- are components. Components are basically wrapper for a `draw` function who are
-- nothing but a `Writer` for instructions. They also have access to a rendering
-- state (the `DrawingContext`) which lets you know the available space to draw, or
-- store the actual position and space taken by a component for future use (mostly
-- event handling).
--
-- Components should be composable. E.g.:
--
-- @
-- renderSomething =
--   let borderColours = Colours (Just blue) (Just black)
--       textColour = Colours (Just white) (Just black)
--   in hBox [
--     bordered borderColours . padded 3 $ label "Hello" TLeft textColour
--   ]
-- @
--
-- Components should always be wrapped in a layout component, that will handle
-- the nitty-gritty of giving them a dedicated console with a dedicated size.
-- Default available layout components are `hBox`, `vBox` and a more
-- complex `grid`. The layout computation is fairly simple. That is because
-- Rogui is not a full GUI toolbox (unlike Brick), but rather a toolkit to
-- build roguelikes, where the UI is normally not too complicated.
--
-- Components _do not_ store their state. It is a responsability of your
-- application layer. Some of the predefined components (like `list`, `grid` or
-- `viewport`) come with their own sub-state datatype and event handling
-- functions that you can use, but you will have to store it in your application
-- state.
--
-- Finally, note that components are parametric over a type `n`, which you'll
-- have to define yourself for your application. It is typically an enumerated
-- type, which will let you:
--
-- - Maintain a FocusRing for focusable components;
-- - Store the actual size of the components post-rendering so it can
--   be retrieved when handling events and identify if a component was clicked.

import Rogui.Components.Button
import Rogui.Components.Core
import Rogui.Components.Grid
import Rogui.Components.Label
import Rogui.Components.MessageLog
import Rogui.Components.MultilineText
import Rogui.Components.ProgressBar
import Rogui.Components.TextInput
import Rogui.Components.Types
