{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}

--
-- @

-- | This module contains the main entry point to use. Use either `boot` or `bootAndPrintError`
-- to start a Rogui application. They will both expect a function to add brushes and
-- console before actually starting the game loop.
--
-- == (Very) Quick start
--
-- Rogui expects you to provide a few types by yourself.
--
-- To boot, you'll need some references for consoles and for brushes:
--
-- @
--
-- data Consoles = Root | GameGrid | TopBar | ModalWindow
-- data Brushes = Charset | Tileset
--
-- @
--
-- You also need a type that represents your application state. In this example,
-- since we just want to compile, we'll keep things super simple and use `()`.
--
-- (You'll also most likely need other types for Event and Component managements, see
-- `Rogui.Application.Events` and `Rogui.Components`)
--
-- You'll want to begin with a `RoguiConfig` object to define your application.
--
-- @
--
-- let t10x16 = TileSize 10 16
-- let t16x16 = TileSize 16 16
-- let conf = RoguiConfig
--   { -- The default brush tile size
--     brushTilesize = ts10x16,
--     -- Name for your app, will be the window title
--     appName = "My app name",
--     -- The full window size, given in cells with the default brush tile size.
--     consoleCellSize = V2 50 38,
--     -- The FPS to run at. Main loop will sleep to avoid going above.
--     targetFPS = 60,
--     -- One of the `Consoles` constructor we've defined above
--     rootConsoleReference = Root,
--     -- One of the `Brushes` constructor we've defined above
--     defaultBrushReference = Charset,
--     -- Path to the resource. Left for a Bytestring, Right for a filepath.
--     defaultBrushPath = Right "tileset.png",
--     -- Transparency colour (if any) for your brush. Charset often use black.
--     defaultBrushTransparencyColour = Just black
--     -- We'll describe this one later
--     drawingFunction = const [],
--     -- A custom step that will fire a "Step" event you can use.
--     stepMs = 100,                               -
--     -- How to react to event. `baseEventHandler` ensures you can quit the app.
--     -- You'll learn more about this in the Events module.
--     eventFunction = baseEventHandler,
--     -- This lets you define all the consoles from which you'll display
--     game and UI elements.
--     consoleSpecs =
--       [ (TopBar, ts10Xx16, TileSize 100 2, TopLeft)
--       , (GameGrid, ts16x16, SizeWindowPct 100 98, Below TopBar)
--       , (ModalWindow, ts10x16, SizeWindowPct 80 80, Center)
--       ]
--     -- And this let you load additional tilesets or charsets (called Brush
--     in Rogui) that you might need.
--     brushesSpecs =
--       [ (TileSet, pure black, ts10x16, Right "path_other_tileset.png") ]
-- }
--
-- @
--
-- The `drawingFunction` will receive the map of known brushes. And you're expected to return
-- a list of triplets: `[(Consoles, Brushes, Component n)]`. See the `Rogui.Components` module
-- for more details on the last type. Consoles will be drawn in the given order, so you
-- typically want to keep them in your expected z-order (e.g.: modals should come last).
-- In "real" applications, it often useful to have a `catMaybes` and a bunch of conditional
-- to know if you want to display a given console or not. Here, we'll just return an empty
-- map so that it compiles.
--
-- Pay attention to the fact that consoles have an expected tilesize. Here, we can use
-- our `TileSet` brush on the `GameGrid` console, but not on the other consoles (an
-- exception will be raised if we try). Conversely, we cannot use our `Charset` brush (which
-- got automatically loaded from our `RoguiConfig` definition) on `GameGrid`.
--
-- But this doesn't matter in this example, because we're not rendering
-- anything, just showcasing how to get rogui to boot and display a window.
--
-- With the configuration datatype and the loading function defined, we're ready
-- to actually boot.  Rogui exposes two functions for this: `boot` and
-- `bootAndPrintError`. The first one expects you to provide logging and error
-- management. We'll use the latter that is designed for quick experiments.
--
-- @ bootAndPrintError config makeGui () @
--
-- This will return a window wih a black background. You can quit by pressing
-- CTRL+C.  Reading on `Rogui.Components` will teach you how you can actually
-- use the `drawingFunction` to render stuff.
module Rogui.Application.System
  ( -- * Main entry point
    boot,
    bootAndPrintError,

    -- * Log wrapper
    LogOutput (..),
    withLogging,
    withoutLogging,

    -- * Other utilities
    brushLookup,

    -- * Reexport for convenience
    module Rogui.Application.Types,
    module Rogui.Application.ConsoleSpecs,
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.State.Strict hiding (state)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Writer.Strict
import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.Foldable (traverse_)
import Data.Map qualified as M
import Data.Maybe (catMaybes)
import Data.Sequence qualified as Seq
import Data.Set qualified as S
import Data.String
import Data.Text (pack)
import Data.Word
import Rogui.Application.ConsoleSpecs
import Rogui.Application.Error (RoguiError (..), TileSizeMismatch (..))
import Rogui.Application.Event
import Rogui.Application.Types (ConsoleSpec, RoguiConfig (..))
import Rogui.Components.Types
import Rogui.Graphics
import Rogui.Types (BrushSpec, PositionSpec (..), Rogui (..), SizeSpec (..))
import SDL (MouseMotionEventData (MouseMotionEventData))
import SDL qualified
import SDL.Event (MouseButtonEventData (..))
import SDL.Image qualified as SDL
import SDL.Internal.Numbered qualified as Numbered
import SDL.Raw qualified as Raw

-- | How to output the logs: either directly to console or to
-- a filepath.
data LogOutput = LogStdout | LogFile FilePath

-- | Run an action with the specified logging output.
-- This unwraps the LoggingT transformer based on the LogOutput choice.
withLogging :: (MonadBaseControl IO m, MonadIO m) => LogOutput -> LoggingT m a -> m a
withLogging ls f = case ls of
  LogStdout -> runStdoutLoggingT f
  LogFile filepath -> runFileLoggingT filepath f

-- | Run an action without logging (discards all log messages).
withoutLogging :: (MonadIO m) => NoLoggingT m a -> m a
withoutLogging = runNoLoggingT

convertSurface :: (MonadIO m) => SDL.Surface -> SDL.PixelFormat -> m SDL.Surface
convertSurface (SDL.Surface s _) pixFmt = do
  fmt <- Raw.allocFormat (Numbered.toNumber pixFmt)
  surface <- SDL.Surface <$> Raw.convertSurface s fmt 0 <*> pure Nothing
  surface <$ Raw.freeFormat fmt

loadBrush :: (MonadIO m, MonadLogger m) => SDL.Renderer -> Maybe RGBA -> Either ByteString FilePath -> TileSize -> m Brush
loadBrush renderer transparency method TileSize {..} = do
  fontSurface <- case method of
    Left bs -> SDL.decode bs
    Right fp -> logDebugNS "Rogui.System" ("Loading brush at " <> fromString fp) >> SDL.load fp

  surface <- convertSurface fontSurface SDL.RGBA8888
  void $ SDL.surfaceColorKey surface SDL.$= transparency
  brush <- SDL.createTextureFromSurface renderer surface
  textInfo <- SDL.queryTexture brush
  pure $
    Brush
      { tileWidth = pixelWidth,
        tileHeight = pixelHeight,
        textureWidth = fromIntegral (SDL.textureWidth textInfo),
        textureHeight = fromIntegral (SDL.textureHeight textInfo),
        brush
      }

-- Load and store a brush into a `Rogui` datatype.
addBrush ::
  (MonadIO m, MonadLogger m, Ord rb) =>
  -- | Brush reference constructor
  rb ->
  -- | Transparency colour used in this brush, if any
  Maybe RGBA ->
  -- | Filepath to the brush
  Either ByteString FilePath ->
  -- | Expected Tilesize for the brush
  TileSize ->
  -- | Rogui object where the brush will be loaded
  Rogui rc rb n s e m' ->
  m (Rogui rc rb n s e m')
addBrush ref transparency path tileSize rogui@Rogui {..} = do
  brush <- loadBrush renderer transparency path tileSize
  pure $ rogui {brushes = M.insert ref brush brushes}

-- | Store a console to be reused later on. Each console are registered with a
-- given `rc` type (for References Console). You should typically prefer
-- `addConsoleWithSpecs` (see `ConsoleSpecs` module).
addConsole :: (Ord rc) => rc -> Console -> Rogui rc rb n s e m -> Rogui rc rb n s e m
addConsole ref console rogui@Rogui {..} =
  rogui {consoles = M.insert ref console consoles}

-- | A helper to define a Console to be stored in the console list and
-- referenced later, using your `rc` (References Console) type.
-- This is to be used in the function expected by `boot`.
--
-- Note that trying to use a brush with a different tilesize than the one
-- expected by the Console will raise an exception.
--
-- See `ConsoleSpecs` to see the available constructor for size and position
-- helpers.
--
-- Example:
--
-- @
-- addConsoles rogui =
--   addConsoleWithSpec TopBarConsole (TileSize 10 16) (SizeWindowPct 100 2) TopLeft rogui
--   >>= addConsoleWithSpec GameGrid (TileSize 16 16) (SizeWindowPct 100 98) (Below StatusBar)
-- @
buildConsoleFromSpecs ::
  (Ord rc, MonadError (RoguiError err rc rb) m) =>
  -- | The size of the console cells, expressed in pixels
  TileSize ->
  -- | How should the size of this console be computed ?
  SizeSpec ->
  -- | Where should this console be positioned ?
  PositionSpec rc ->
  -- | The rogui datatype that will store this Console
  Rogui rc rb n s e m' ->
  m Console
buildConsoleFromSpecs consoleTS sizeSpec posSpec rogui@Rogui {rootConsole} = do
  let Console {..} = rootConsole
      (w, h) = case sizeSpec of
        FullWindow -> (width, height)
        SizeWindowPct wp hp -> (width * Pixel wp `div` 100, height * Pixel hp `div` 100)
        TilesSize tw th -> (pixelWidth consoleTS .*=. tw, pixelHeight consoleTS .*=. th)
        PixelsSize pw ph -> (pw, ph)
      pos = case posSpec of
        TopLeft -> pure $ SDL.V2 0 0
        TopRight -> pure $ SDL.V2 (width - w) 0
        BottomLeft -> pure $ SDL.V2 0 (height - h)
        BottomRight -> pure $ SDL.V2 (width - w) (height - h)
        Center -> pure $ SDL.V2 ((width - w) `div` 2) ((height - h) `div` 2)
        PosWindowPct xp yp -> pure $ SDL.V2 (width * Pixel xp `div` 100) (height * Pixel yp `div` 100)
        TilesPos tx ty -> pure $ SDL.V2 (pixelWidth consoleTS .*=. tx) (pixelHeight consoleTS .*=. ty)
        PixelsPos px py -> pure $ SDL.V2 px py
        Below rc -> consoleBelow rc rogui
        RightOf rc -> consoleRight rc rogui
  (Console w h <$> pos) <*> pure consoleTS

applyBrushSpecs ::
  (MonadLogger m, MonadIO m, MonadError (RoguiError err rc rb) m, Ord rb) =>
  [BrushSpec rb] ->
  Rogui rc rb n s e m ->
  m (Rogui rc rb n s e m)
applyBrushSpecs specs rogui =
  let foldSpecs rogui' (ref, tpColour, tileSize, loadingMethod) =
        addBrush ref tpColour loadingMethod tileSize rogui'
   in foldM foldSpecs rogui specs

applyConsoleSpecs ::
  (MonadError (RoguiError err rc rb) m, Ord rc) =>
  Console ->
  [ConsoleSpec rc] ->
  Rogui rc rb n s e m ->
  m (Rogui rc rb n s e m)
applyConsoleSpecs root specs rogui@Rogui {..} =
  let foldSpecs rogui' (ref, consoleTS, sizeSpec, posSpec) = do
        newConsole <- buildConsoleFromSpecs consoleTS sizeSpec posSpec rogui'
        pure $ addConsole ref newConsole rogui'
   in foldM foldSpecs (rogui {consoles = M.singleton rootConsoleRef root}) specs

-- | This is a simplified version of boot, that handles error management and
-- logging for you. It will output any error (without trying to recover) and log
-- to the standard output. This is typically useful while you're developing,
-- or for prototyping, later to be ditched for a call to `boot` with more
-- custom behaviour designed. It will not work if you need a custom error type
-- however.
bootAndPrintError ::
  (Show rc, Show rb, Ord rb, Ord rc, Ord n, MonadIO m) =>
  -- | A Configuration that will be used to make a Rogui datatype
  RoguiConfig rc rb n s e (ExceptT (RoguiError () rc rb) (LoggingT m)) ->
  -- | Initial state
  s ->
  m ()
bootAndPrintError c i = runStdoutLoggingT $ do
  result <- runExceptT (boot c i)
  either (liftIO . print) pure result

-- | This function uses the `RoguiConfig` provided to initialise a `Rogui` datatype,
-- and start the main loop.
-- Boot will return once a `Halt` EventResult has been processed in the event handler.
boot ::
  (Show rb, Ord rb, Ord rc, Ord n, MonadIO m, MonadError (RoguiError err rc rb) m, MonadLogger m) =>
  -- | A Configuration that will be used to make a Rogui datatype
  RoguiConfig rc rb n s e m ->
  -- | Initial state
  s ->
  m ()
boot RoguiConfig {..} initialState = do
  SDL.initializeAll
  let TileSize {..} = brushTilesize
      (SDL.V2 widthInTiles heightInTiles) = consoleCellSize
  let windowSize@(SDL.V2 w h) = SDL.V2 (pixelWidth .*=. widthInTiles) (pixelHeight .*=. heightInTiles)
  window <-
    SDL.createWindow
      appName
      SDL.defaultWindow
        { SDL.windowInitialSize = fromIntegral <$> windowSize,
          SDL.windowResizable = allowResize
        }
  when allowResize
    . void
    $ SDL.windowMinimumSize window SDL.$= (fromIntegral <$> windowSize)
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  baseBrush <- loadBrush renderer defaultBrushTransparencyColour defaultBrushPath brushTilesize
  let baseConsole = Console {width = w, height = h, position = SDL.V2 0 0, tileSize = brushTilesize}
      frameTime = 1000 `div` fromIntegral targetFPS -- milliseconds per frame
      baseRogui =
        Rogui
          { consoles = M.singleton rootConsoleReference baseConsole,
            brushes = M.singleton defaultBrushReference baseBrush,
            rootConsoleRef = rootConsoleReference,
            rootConsole = baseConsole,
            defaultBrush = baseBrush,
            renderer = renderer,
            draw = drawingFunction,
            onEvent = eventFunction,
            lastTicks = 0,
            timerStep = stepMs,
            lastStep = 0,
            numberOfSteps = 0,
            targetFrameTime = frameTime,
            extentsMap = mempty,
            recentFrameTimes = mempty,
            lastFPSWarning = 0,
            roguiConsoleSpecs = consoleSpecs
          }
  withConsoles <- applyConsoleSpecs baseConsole consoleSpecs baseRogui
  withBrushes <- applyBrushSpecs brushesSpecs withConsoles

  appLoop withBrushes initialState

  SDL.destroyWindow window

-- | Lookup a brush by its reference. Throw an error if not found.
brushLookup :: (Ord rb, Show rb, MonadError (RoguiError err rc rb) m) => M.Map rb Brush -> rb -> m Brush
brushLookup m ref =
  maybe (throwError $ NoSuchBrush ref) pure (M.lookup ref m)

-- | Calculate current FPS from a window of recent frame times.
-- Returns Nothing if there aren't enough samples yet.
calculateFPS :: Seq.Seq Word32 -> Int -> Maybe Double
calculateFPS frameTimes minSamples
  | Seq.length frameTimes < minSamples = Nothing
  | otherwise =
      let totalTime = sum frameTimes
          avgFrameTime = fromIntegral totalTime / fromIntegral (Seq.length frameTimes) :: Double
       in Just (1000.0 / avgFrameTime)

applyResize :: (Ord rc, MonadError (RoguiError err rc rb) m) => SDL.V2 Cell -> Rogui rc rb n s e m -> m (Rogui rc rb n s e m)
applyResize (SDL.V2 newW newH) r@Rogui {..} =
  let Console {..} = rootConsole
      newRoot =
        rootConsole
          { width = pixelWidth tileSize .*=. newW,
            height = pixelHeight tileSize .*=. newH
          }
   in applyConsoleSpecs newRoot roguiConsoleSpecs $ r {rootConsole = newRoot}

-- Get the SDL events; we will process the resizing events in there,
-- so we can rebuild a Rogui datatype properly to react to this particular event.
-- This also removes duplicated input events, so they don't build up faster
-- than rendering.
preEventLoop :: (Ord rc, MonadIO m, MonadError (RoguiError err rc rb) m, MonadLogger m) => Rogui rc rb n s e m -> m (Rogui rc rb n s e m, [Event e])
preEventLoop initialRogui@Rogui {..} = do
  sdlEventsWithResize <- getSDLEvents defaultBrush
  let eventFolder (r, evs) ev@(WindowResized newSize) = (,) <$> applyResize newSize r <*> pure (ev : evs)
      eventFolder (r, evs) other = pure (r, other : evs)
  (finalRogui, processedEvents) <- foldM eventFolder (initialRogui, []) sdlEventsWithResize
  pure (finalRogui, reverse processedEvents)

appLoop :: (Show rb, Ord rb, Ord rc, Ord n, MonadIO m, MonadError (RoguiError err rc rb) m, MonadLogger m) => Rogui rc rb n s e m -> s -> m ()
appLoop initialGui state = do
  frameStart <- SDL.ticks
  (roGUI@Rogui {..}, sdlEvents) <- preEventLoop initialGui
  let reachedStep = frameStart - lastStep > timerStep
      baseEvents = if reachedStep then Step : sdlEvents else sdlEvents
      baseEventState = EventHandlingState {events = Seq.fromList baseEvents, currentState = state, result = ContinueNoRedraw, knownExtents = extentsMap}
  EventHandlingState {result, currentState} <- execStateT (processWithLimit 30 roGUI) baseEventState
  newExtents <-
    if result == Continue
      then do
        SDL.clear renderer
        let drawConsole (console, brush, components) = do
              let onConsole = maybe rootConsole (consoles M.!) console
              usingBrush <- maybe (pure defaultBrush) (brushLookup brushes) brush
              renderComponents roGUI usingBrush onConsole components
        extents <- traverse drawConsole (draw brushes currentState)
        SDL.present renderer
        pure $ M.unions extents
      else pure extentsMap
  unless (result == Halt) $ do
    frameEnd <- SDL.ticks
    let frameDuration = frameEnd - frameStart
        elapsed = frameStart - frameEnd
        sleepMs =
          if elapsed < targetFrameTime
            then targetFrameTime - elapsed
            else 0
    liftIO $ threadDelay (fromIntegral sleepMs * 1000)

    -- FPS tracking and logging
    let maxFrameSamples = 60
        newFrameTimes = Seq.take maxFrameSamples (frameDuration Seq.<| recentFrameTimes)
        targetFPS' = fromIntegral (1000 `div` targetFrameTime) :: Double
        threshold = targetFPS' * 0.8
        fpsWarningCooldown = 5000 :: Word32 -- 5 seconds between warnings
        shouldWarn = do
          currentFPS <- calculateFPS newFrameTimes maxFrameSamples
          if currentFPS < threshold && (frameEnd - lastFPSWarning) > fpsWarningCooldown
            then Just currentFPS
            else Nothing
        newLastFPSWarning = case shouldWarn of
          Just _ -> frameEnd
          Nothing -> lastFPSWarning
    case shouldWarn of
      Just currentFPS ->
        logWarnN $
          "Low FPS detected: "
            <> pack (show (round currentFPS :: Int))
            <> " (target: "
            <> pack (show (round targetFPS' :: Int))
            <> ")"
      Nothing -> pure ()

    let newRogui =
          roGUI
            { lastTicks = frameStart,
              lastStep = if reachedStep then frameStart else lastStep,
              numberOfSteps = if reachedStep then numberOfSteps + 1 else numberOfSteps,
              extentsMap = newExtents,
              recentFrameTimes = newFrameTimes,
              lastFPSWarning = newLastFPSWarning
            }
    appLoop newRogui currentState

processWithLimit :: (Monad m) => Int -> Rogui rc rb n s e m -> EventHandlingM m s e n ()
processWithLimit 0 _ = pure ()
processWithLimit n roGUI = do
  evs <- gets events
  if null evs
    then pure ()
    else traverse_ (processEvent roGUI) evs >> processWithLimit (n - 1) roGUI

popEvent :: (Monad m) => EventHandlingM m state e n (Maybe (Event e))
popEvent = do
  currentEvents <- gets events
  case currentEvents of
    Seq.Empty -> pure Nothing
    (firstEvent Seq.:<| rest) -> do
      modify $ \ehs -> ehs {events = rest}
      pure $ Just firstEvent

processEvent :: (Monad m) => Rogui rc rb n s e m -> Event e -> EventHandlingM m s e n ()
processEvent Rogui {..} event = do
  _ <- popEvent -- This will remove the event from the queue
  currentState <- gets currentState
  void . runEventHandler . onEvent currentState $ event
  pure ()

getSDLEvents :: (MonadIO m) => Brush -> m [Event e]
getSDLEvents Brush {..} =
  let toRoguiEvent = \case
        SDL.KeyboardEvent ke -> case SDL.keyboardEventKeyMotion ke of
          SDL.Pressed -> KeyDown $ KeyDownDetails (SDL.keyboardEventRepeat ke) (keysymToKeyDetails $ SDL.keyboardEventKeysym ke)
          SDL.Released -> KeyUp . keysymToKeyDetails $ SDL.keyboardEventKeysym ke
        SDL.WindowSizeChangedEvent (SDL.WindowSizeChangedEventData _ (SDL.V2 w h)) ->
          WindowResized (SDL.V2 (fromIntegral w ./.= tileWidth) (fromIntegral h ./.= tileHeight))
        SDL.MouseMotionEvent MouseMotionEventData {..} ->
          let (SDL.P mousePos) = mouseMotionEventPos
              absoluteMousePosition@(SDL.V2 x y) = fromIntegral <$> mousePos
              defaultTileSizePosition = SDL.V2 (x ./.= tileWidth) (y ./.= tileHeight)
              relativeMouseMotion = fromIntegral <$> mouseMotionEventRelMotion
           in MouseEvent . MouseMove $ MouseMoveDetails {..}
        SDL.MouseButtonEvent MouseButtonEventData {..} ->
          let (SDL.P mousePos) = mouseButtonEventPos
              absoluteMousePosition@(SDL.V2 x y) = fromIntegral <$> mousePos
              defaultTileSizePosition = SDL.V2 (x ./.= tileWidth) (y ./.= tileHeight)
              buttonClicked = mouseButtonEventButton
              constructor = if mouseButtonEventMotion == SDL.Released then MouseClickReleased else MouseClickPressed
           in MouseEvent . constructor $ MouseClickDetails {..}
        e -> OtherSDLEvent e
      deduplicatedPayload es = S.toList . S.fromList $ fmap SDL.eventPayload es
   in fmap (fmap toRoguiEvent) (deduplicatedPayload <$> SDL.pollEvents)

keysymToKeyDetails :: SDL.Keysym -> KeyDetails
keysymToKeyDetails SDL.Keysym {..} =
  let toModifier SDL.KeyModifier {..} =
        S.fromList . catMaybes $
          [ if keyModifierLeftShift then Just Shift else Nothing,
            if keyModifierRightShift then Just Shift else Nothing,
            if keyModifierLeftCtrl then Just Ctrl else Nothing,
            if keyModifierRightCtrl then Just Ctrl else Nothing,
            if keyModifierLeftAlt then Just Alt else Nothing,
            if keyModifierRightAlt then Just Alt else Nothing,
            if keyModifierAltGr then Just Alt else Nothing
          ]
   in KeyDetails keysymKeycode keysymScancode (toModifier keysymModifier)

renderComponents :: (Ord n, MonadIO m, MonadError (RoguiError err rc rb) m) => Rogui rc rb n s e m' -> Brush -> Console -> Component n -> m (ExtentMap n)
renderComponents Rogui {defaultBrush, rootConsole, numberOfSteps, renderer} usingBrush usingConsole@Console {tileSize = consoleTileSize} Component {..} = do
  let brushTileSize = fromBrush usingBrush
  when (brushTileSize /= consoleTileSize) $
    throwError $
      BrushConsoleMismatch (TileSizeMismatch consoleTileSize brushTileSize)
  let afterRendering =
        execStateT
          rendering
          DrawingContext {brush = usingBrush, console = usingConsole, steps = numberOfSteps, currentExtents = mempty}
      (extents, instructions) =
        first currentExtents $ runWriter afterRendering
      rendering = do
        withConsole usingConsole
        withBrush usingBrush
        draw
  evalInstructions renderer rootConsole defaultBrush instructions >> pure extents