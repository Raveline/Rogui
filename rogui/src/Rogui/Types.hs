-- | This module exposes some of the basic types used when handling
-- the runtime Rogui.
--
-- You never create the `Rogui` datatype yourself; it gets generated when you use
-- one of the boot function from `Rogui.Application.System`. The `Rogui` object
-- is used mostly internally to handle all the information needed by the game
-- loop. It is built through the `Rogui.Application.Config.RoguiConfig` type.
--
-- = Game definition through Rogui
--
-- == Rogui type parameters
--
-- `Rogui` is parametric over 8 types, which you are all expected to provide:
--
-- * The `rc` type `references consoles`: typically an enumerated tile, with an `Ord` instance.
-- * The `rb` type `references brushes`: similar to the previous one, but for brushes.
-- * The `n` type for `Name`: it lets you name some components, which is used for focus management and some events management.
-- * The `s` type for `State`: your application state.
-- * The `e` type for `Event`: your custom events types.
-- * The `r` type for `Renderer`: this one is defined by the backend you are using;
-- * The `t` type for `Texture`: again, defined by the backend in use;
-- * The `m` type: your monadic stack. If you don't need any effect, you can leave it as `m`.
--
-- Here are a few typical examples for these:
--
-- @
--
-- data Consoles = Root | GameArea | SideBar | CenteredWindow
-- data Brushes = Charset | DungeonTileset | MonsterTileset | ForestTileset
-- data Names = ValidateButton | PickItemList | CharacterNameInput
-- data AppState = AppState {
--   dungeon :: Array (V2 Cell) Tiles
--   playerPos :: V2 Cell,
--   playerInventory :: M.Map SomeObjectType Int
--   monsters :: [(SomeMonsterInfoType, V2 Cell)]
--   objects :: [(SomeObjects, V2 Cell)]
--   gameState :: SomeGameStateEnum
-- }
-- data Event = PlayerMove (V2 Cell) | MonsterAttack SomeMonsterInfoType -- ... and so on
--
-- @
--
-- You plug your game logic in two main points:
--
-- * The `draw` function handles rendering, through components.
-- * The `onEvent` function handles reacting to events.
--
-- == Drawing
--
-- Drawing takes two parameters:
--
-- * The map of known brushes, so you can access them if you need to
-- switch brushes (typically if you use several tileset).
-- * The current state of the application, which will drive things that
-- should (or should not) be displayed.
--
-- Drawing returns a list of triplets, containing:
--
-- * Maybe a console reference. Default console will be used if nothing is provided;
-- * Maybe a brush reference. Default brush will be used if nothing is provided;
-- * A single component. This will normally be a layout component containing
-- many other children (see `Rogui.Components.Core.vBox` and `Rogui.Components.Core.hBox`).
--
-- Each triplet will be drawn in the given order. So a window on top of
-- the current game area should be returned last. A simple realistic example could be:
--
-- @
-- draw :: M.Map Brushes -> ApplicationState -> ToDraw Consoles Brushes Names
-- draw _brushes appState =
--   catMaybes $
--     [ Just (Nothing, Nothing, renderGameGrid appState)
--       if (gameState appState == ShowInventory)
--         then Just (Just CenteredWindow, Just Charset, renderInventory (playerInventory appState))
--         else Nothing
--     ]
-- @
--
-- == Reacting to events
--
-- Events are detailed in `Rogui.Application.Event`.
-- The core type for handling events is `Rogui.Application.Event.EventHandler`
-- , a function that takes two parameters:
--
-- * The current application state;
-- * The current event to process;
--
-- And returns an `EventHandlerM`, which is a newtype wrapper that provides:
--
-- * Access to a event handling state, letting you fire new events, access
-- temporary application state changes, access known extents of components,
-- and set a flag letting the loop know if redraw is needed.
-- * A `EventHandlingResult` that lets handler tell if they handled an
-- event or not. This is particularly useful to chain event handlers through
-- `Alternative`. When your handler cannot process an event, it should return
-- `unhandled`.
--
-- Some default or simple event handlers are provided by Rogui. The most
-- common one is `baseEventHandler`, which handles some basic events like
-- quitting the application. Another common one is `keyEventHandler`, which
-- lets you provide a map to react to key presses and modifiers.
--
-- It should be noted however that Rogui expects you to do a lot of the
-- work when it comes to event management. You are responsible, for instance,
-- for plugging the right handlers depending on your application state;
-- or for managing focus. We provide utilities to simplify this, but you
-- still need to connect the dots.
--
-- Here is a basic example which showcase:
-- * how to use `FocusRing` with events;
-- * how to chain handlers;
-- * how to use some of the event handlers provided by Rogui base components
--
-- @
--
-- data Names = SomeText | SomeButton
-- data AnEventType = ButtonPressEvent
-- data ApplicationState = ApplicationState {
--   ring :: FocusRing Names,
--   someText :: String,
-- }
--
-- eventHandler :: EventHandler ApplicationState e Names
-- eventHandler =
--   basicEventHandler <||> handleFocusedComponent <||> handleFocusChange
--
-- handleFocusedComponent :: EventHandler ApplicationState e Names
-- handleFocusedComponent s e = case focusGetCurrent (ring s) of
--    Just SomeText ->
--      handleTextInputEvent e (someText s) (\t s' -> s' { someText = t })
--    Just SomeButton ->
--      handleButtonEvent (AppEvent ButtonPressEvent) e
--
-- handleFocusChange :: EventHandler ApplicationState e Names
-- handleFocusChange s e = case e of
--   FocusNext -> modifyState (\s -> s { ring = focusNext (ring s)})
--   FocusPrev -> modifyState (\s -> s { ring = focusPrev (ring s)})
--
-- @
--
-- Several observations on this example:
--
-- * If you noticed we don't handle the `ButtonPressEvent` (we fire it, but it's
-- never handled), bravo, it means you've understood the event handling logic
-- well enough. This could be done by chaining yet another handler in
-- `eventHandler`, for instance.
-- * The state modifiers you have to pass through can get annoying quickly,
-- particularly when you have deeply nested records. In practice, we recommend
-- using lenses (in whatever form you prefer). Rogui is agnostic when it comes
-- to lenses and don't provide any, but that doesn't mean you can't use them.
module Rogui.Types
  ( -- * Main application type
    Rogui (..),

    -- * Event handling
    EventHandler,

    -- * Drawing
    ConsoleDrawers,

    -- * Console specification
    ConsoleSpec,
    PositionSpec (..),
    SizeSpec (..),

    -- * Brush specification
    BrushSpec,

    -- * Drawing
    ToDraw,
  )
where

import Data.ByteString (ByteString)
import qualified Data.Map as M
import Data.Sequence (Seq)
import Data.Word (Word32)
import Rogui.Application.Event (EventHandler)
import Rogui.Components.Core (Component, ExtentMap)
import Rogui.Graphics

-- | Instructions on how to build a console. Expects:
-- * A reference to store the console;
-- * An expected tilesize for brushes that will paint on this console;
-- * A size specification (see `SizeSpec`);
-- * A position specification (see `PositionSpec`).
type ConsoleSpec rc = (rc, TileSize, SizeSpec, PositionSpec rc)

-- | Instructions on how to load a brush. Expects:
-- * A reference to store the brush;
-- * An optional transparency colour;
-- * A tilesize;
-- * A way to load it (either a filepath or a bytestring containing the file).
type BrushSpec rb = (rb, Maybe RGBA, TileSize, Either ByteString FilePath)

-- | Drawing function type. Drawing functions have access to all registered
-- brushes (but if you need to switch brush on a given console,
-- you must fetched them manually when describing the expected UI).
type ConsoleDrawers rc rb n state = M.Map rb Brush -> state -> ToDraw rc rb n

-- | Expected result of the drawing function. A list of triplets containing:
--
-- * A console to use (`rootConsole` if nothing was provided);
-- * A brush to use (`defaultBrush` if nothing was provided);
-- * A component tree.
--
-- Be careful: if the brush you use doesn't match the expected tilesize in
-- the console, an exception will be thrown.
type ToDraw rc rb n = [(Maybe rc, Maybe rb, Component n)]

-- | Rogui is the main datatype used to define an application.
-- It is parametric over:
--
-- * Consoles reference enum `rc`;
-- * Brushes reference enum `rb`;
-- * Components naming enum `n`;
-- * A `s` state;
-- * A custom event type `e`.
-- * Your monadic stack `m`.
--
-- Clients are not expected to build this manually. `boot` will handle it for
-- the users, and will fill (and handle) all the internal fields.
data Rogui rc rb names state event renderer textures m
  = Rogui
  { -- | All known consoles. This gets reconstructed when window is resized.
    consoles :: M.Map rc Console,
    -- |  All known brushes. This gets built at boot.
    brushes :: M.Map rb Brush,
    -- | The root console, for fast access and to ensure there's always a fallback
    rootConsole :: Console,
    -- | The root console reference, need when rebuilding the map of consoles
    rootConsoleRef :: rc,
    -- | The default brush, useful as fallback or for games where only one brush is used
    defaultBrush :: Brush,
    -- | Inner storage for the SDL renderer
    renderer :: renderer,
    -- | Function to draw
    draw :: ConsoleDrawers rc rb names state,
    -- | Main game logic is stored as reactions to events
    onEvent :: EventHandler m state event names,
    -- | Maximum amount of iteration to process events in a single frame
    maxEventDepth :: Int,
    -- | Constant evaluating the amount of milliseconds since initialisation, taken from SDL.
    lastTicks :: Word32,
    -- | Step timer constant used for basic animations, expressed in milliseconds.
    -- Every time the number of milliseconds reach this, we will fire a Step event.
    timerStep :: Word32,
    -- | Inner value to keep track of steps
    lastStep :: Word32,
    -- | Number of steps taken since the beginning of the application.
    numberOfSteps :: Int,
    -- | Internal, milliseconds per frame
    targetFrameTime :: Word32,
    -- | List of known extents
    extentsMap :: ExtentMap names,
    -- | Rolling window of recent frame durations (in milliseconds) for FPS tracking
    recentFrameTimes :: Seq Word32,
    -- | Timestamp of last FPS warning (for rate limiting)
    lastFPSWarning :: Word32,
    -- | Console specs, kept around when resizing needs them
    roguiConsoleSpecs :: [ConsoleSpec rc],
    textures :: M.Map Brush textures
  }

-- | How to size a console.
data SizeSpec
  = -- | Same size as the root console, takes all window
    FullWindow
  | -- | Percentage of width and height of the root window
    SizeWindowPct Int Int
  | -- | Direct definition in width and height in cells
    TilesSize Cell Cell
  | -- | Exact pixels
    PixelsSize Pixel Pixel

-- | Where to put a console.
data PositionSpec rc
  = TopLeft
  | TopRight
  | BottomLeft
  | BottomRight
  | Center
  | -- | Percentages (x and y) from top-left
    PosWindowPct Int Int
  | -- | Position in tiles
    TilesPos Cell Cell
  | -- | Exact pixels
    PixelsPos Pixel Pixel
  | -- | Below another console (stacking)
    Below rc
  | -- | Right of another console (side-by-side)
    RightOf rc