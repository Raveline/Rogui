{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}

module Rogui.Application.System
  ( -- * Main entry point
    boot,
    bootAndPrintError,
    addBrush,
    addConsole,

    -- * Log wrapper
    LogOutput (..),
    withLogging,
    withoutLogging,

    -- * Event handling utilities
    keyPressHandler,
    baseEventHandler,

    -- * Other utilities
    brushLookup,
    calculateFPS,
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.State hiding (state)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Foldable (traverse_)
import Data.Map qualified as M
import Data.Maybe (catMaybes, fromMaybe)
import Data.Sequence qualified as Seq
import Data.Set qualified as S
import Data.String
import Data.Text (pack)
import Data.Word
import Rogui.Application.Error (RoguiError)
import Rogui.Application.Event
import Rogui.Application.Types (RoguiConfig (..))
import Rogui.Components (renderComponents)
import Rogui.Graphics.Types (Brush (..), Console (..), TileSize (..), (.*=.), (./.=))
import Rogui.Types (Rogui (..))
import SDL (MouseMotionEventData (MouseMotionEventData))
import SDL qualified
import SDL.Event (MouseButtonEventData (..))
import SDL.Image qualified as SDL
import SDL.Internal.Numbered qualified as Numbered
import SDL.Raw qualified as Raw

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

loadBrush :: (MonadIO m, MonadLogger m) => SDL.Renderer -> FilePath -> TileSize -> m Brush
loadBrush renderer path TileSize {..} = do
  logDebugN $ "Loading brush at " <> fromString path
  fontSurface <- SDL.load path
  surface <- convertSurface fontSurface SDL.RGBA8888
  let black :: SDL.V4 Word8
      black = SDL.V4 0 0 0 0x00
  void $ SDL.surfaceColorKey surface SDL.$= pure black
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

-- | Load and store a brush into a `Rogui` datatype.
--
-- Expected to be chained monadically like this:
--
-- @
-- prepareRogui :: (MonadIO m) => Rogui rc rb n s e -> m (Rogui rc rb n s e)
-- prepareRogui baseGui = do
--   addBrush Enum1 "pathToBrush.png" (TileSize 16 16) baseGui
--   >>= addBrush BigCharset "terminal_16x16.png" (TileSize 16 16)
-- @
addBrush ::
  (MonadIO m, MonadLogger m, Ord rb) =>
  -- | Brush reference constructor
  rb ->
  -- | Filepath to the brush
  FilePath ->
  -- | Expected Tilesize for the brush
  TileSize ->
  -- | Rogui object where the brush will be loaded
  Rogui rc rb n s e ->
  m (Rogui rc rb n s e)
addBrush ref path tileSize rogui@Rogui {..} = do
  brush <- loadBrush renderer path tileSize
  pure $ rogui {brushes = M.insert ref brush brushes}

addConsole :: (Ord rc) => rc -> Console -> Rogui rc rb n s e -> Rogui rc rb n s e
addConsole ref console rogui@Rogui {..} =
  rogui {consoles = M.insert ref console consoles}

-- | A utility for exiting at the first error and outputing it.
-- Automatically runs with stdout logging.
bootAndPrintError ::
  (Show rc, Show rb, Ord rb, Ord rc, Ord n) =>
  RoguiConfig rc rb n s e ->
  (Rogui rc rb n s e -> ExceptT (RoguiError rc rb) (LoggingT IO) (Rogui rc rb n s e)) ->
  s ->
  IO ()
bootAndPrintError c b i = runStdoutLoggingT $ do
  result <- runExceptT (boot c b i)
  either (liftIO . print) pure result

-- | This function uses the `RoguiConfig` provided to initialise a `Rogui` datatype.
-- This Rogui can then be altered in the function passed as parameter,
-- before being run in the appLoop (starting with the initial state provided as last parameter).
-- Boot will return once a `Halt` EventResult has been processed in the event handler.
boot ::
  (Show rb, Ord rb, Ord rc, Ord n, MonadIO m, MonadError (RoguiError rc rb) m, MonadLogger m) =>
  RoguiConfig rc rb n s e ->
  (Rogui rc rb n s e -> m (Rogui rc rb n s e)) ->
  s ->
  m ()
boot RoguiConfig {..} guiBuilder initialState = do
  SDL.initializeAll
  let TileSize {..} = brushTilesize
      (SDL.V2 widthInTiles heightInTiles) = consoleCellSize
  let windowSize@(SDL.V2 w h) = SDL.V2 (pixelWidth .*=. widthInTiles) (pixelHeight .*=. heightInTiles)
  window <- SDL.createWindow appName SDL.defaultWindow {SDL.windowInitialSize = fromIntegral <$> windowSize}
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  baseBrush <- loadBrush renderer defaultBrushPath brushTilesize
  let baseConsole = Console {width = w, height = h, position = SDL.V2 0 0, tileSize = brushTilesize}
      frameTime = 1000 `div` fromIntegral targetFPS -- milliseconds per frame
      baseRogui =
        Rogui
          { consoles = M.singleton rootConsoleReference baseConsole,
            brushes = M.singleton defaultBrushReference baseBrush,
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
            lastFPSWarning = 0
          }
  gui <- guiBuilder baseRogui

  appLoop gui initialState

  SDL.destroyWindow window

-- | Utility to look for a given brush in a map of brushes.
-- This will later be patched to handle error more gracefully.
brushLookup :: (Ord rb, Show rb) => M.Map rb Brush -> rb -> Brush
brushLookup m ref =
  fromMaybe (error $ "Brush not found: " ++ show ref) (M.lookup ref m)

-- | Calculate current FPS from a window of recent frame times.
-- Returns Nothing if there aren't enough samples yet.
calculateFPS :: Seq.Seq Word32 -> Int -> Maybe Double
calculateFPS frameTimes minSamples
  | Seq.length frameTimes < minSamples = Nothing
  | otherwise =
      let totalTime = sum frameTimes
          avgFrameTime = fromIntegral totalTime / fromIntegral (Seq.length frameTimes) :: Double
       in Just (1000.0 / avgFrameTime)

appLoop :: (Show rb, Ord rb, Ord rc, Ord n, MonadIO m, MonadError (RoguiError rc rb) m, MonadLogger m) => Rogui rc rb n s e -> s -> m ()
appLoop roGUI@Rogui {..} state = do
  sdlEvents <- getSDLEvents defaultBrush
  frameStart <- SDL.ticks
  let reachedStep = frameStart - lastStep > timerStep
      baseEvents = if reachedStep then Step : sdlEvents else sdlEvents
      baseEventState = EventHandlingState {events = Seq.fromList baseEvents, currentState = state, result = ContinueNoRedraw, knownExtents = extentsMap}
      EventHandlingState {result, currentState} = execState (processWithLimit 30 roGUI) baseEventState
  newExtents <-
    if result == Continue
      then do
        SDL.clear renderer
        let drawConsole (console, brush, components) = do
              let onConsole = maybe rootConsole (consoles M.!) console
                  usingBrush = maybe defaultBrush (brushLookup brushes) brush
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

processWithLimit :: Int -> Rogui rc rb n s e -> EventHandlingM s e n ()
processWithLimit 0 _ = pure ()
processWithLimit n roGUI = do
  evs <- gets events
  if null evs
    then pure ()
    else traverse_ (processEvent roGUI) evs >> processWithLimit (n - 1) roGUI

popEvent :: EventHandlingM state e n (Maybe (Event e))
popEvent = do
  currentEvents <- gets events
  case currentEvents of
    Seq.Empty -> pure Nothing
    (firstEvent Seq.:<| rest) -> do
      modify $ \ehs -> ehs {events = rest}
      pure $ Just firstEvent

processEvent :: Rogui rc rb n s e -> Event e -> EventHandlingM s e n ()
processEvent Rogui {..} event = do
  _ <- popEvent -- This will remove the event from the queue
  currentState <- gets currentState
  void . runEventHandler . onEvent currentState $ event
  pure ()

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

getSDLEvents :: (MonadIO m) => Brush -> m [Event e]
getSDLEvents Brush {..} =
  let toRoguiEvent (SDL.Event _timestamp payload) = case payload of
        SDL.KeyboardEvent ke -> case SDL.keyboardEventKeyMotion ke of
          SDL.Pressed -> KeyDown $ KeyDownDetails (SDL.keyboardEventRepeat ke) (keysymToKeyDetails $ SDL.keyboardEventKeysym ke)
          SDL.Released -> KeyUp . keysymToKeyDetails $ SDL.keyboardEventKeysym ke
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
              buttonCliked = mouseButtonEventButton
           in MouseEvent . MouseClick $ MouseClickDetails {..}
        e -> OtherSDLEvent e
   in fmap (fmap toRoguiEvent) SDL.pollEvents

keysymToKeyDetails :: SDL.Keysym -> KeyDetails
keysymToKeyDetails SDL.Keysym {..} =
  let toModifier SDL.KeyModifier {..} =
        S.fromList . catMaybes $
          [ if keyModifierLeftShift then Just LeftShift else Nothing,
            if keyModifierRightShift then Just RightShift else Nothing,
            if keyModifierLeftCtrl then Just LeftCtrl else Nothing,
            if keyModifierRightCtrl then Just RightCtrl else Nothing,
            if keyModifierLeftAlt then Just LeftAlt else Nothing,
            if keyModifierRightAlt then Just RightAlt else Nothing,
            if keyModifierAltGr then Just AltGr else Nothing
          ]
   in KeyDetails keysymKeycode (toModifier keysymModifier)