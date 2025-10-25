{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}

module Rogui.Application.System
  ( -- * Main entry point
    boot,
    bootAndPrintError,
    addBrush,
    addConsole,

    -- * Event handling utilities
    keyPressHandler,
    baseEventHandler,

    -- * Other utilities
    brushLookup,
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.State hiding (state)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Sequence qualified as Seq
import Data.Word
import Rogui.Application.Error (RoguiError)
import Rogui.Application.Event
import Rogui.Application.Types (RoguiConfig (..))
import Rogui.Components (renderComponents)
import Rogui.Graphics.Types (Brush (..), Console (..), TileSize (..), (.*=.), (./.=))
import Rogui.Types (EventHandler, Rogui (..))
import SDL (MouseMotionEventData (MouseMotionEventData))
import SDL qualified
import SDL.Event (MouseButtonEventData (..))
import SDL.Image qualified as SDL
import SDL.Internal.Numbered qualified as Numbered
import SDL.Raw qualified as Raw

convertSurface :: (MonadIO m) => SDL.Surface -> SDL.PixelFormat -> m SDL.Surface
convertSurface (SDL.Surface s _) pixFmt = do
  fmt <- Raw.allocFormat (Numbered.toNumber pixFmt)
  surface <- SDL.Surface <$> Raw.convertSurface s fmt 0 <*> pure Nothing
  surface <$ Raw.freeFormat fmt

loadBrush :: (MonadIO m) => SDL.Renderer -> FilePath -> TileSize -> m Brush
loadBrush renderer path TileSize {..} = do
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
  (MonadIO m, Ord rb) =>
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

-- | A utility for exiting at the first error and outputing it
bootAndPrintError ::
  (Show rc, Show rb, Ord rb, Ord rc, Ord n, MonadIO m) =>
  RoguiConfig rc rb n s e ->
  (Rogui rc rb n s e -> ExceptT (RoguiError rc rb) m (Rogui rc rb n s e)) ->
  s ->
  m ()
bootAndPrintError c b i = do
  result <- runExceptT (boot c b i)
  either (liftIO . print) pure result

-- | This function uses the `RoguiConfig` provided to initialise a `Rogui` datatype.
-- This Rogui can then be altered in the function passed as parameter,
-- before being run in the appLoop (starting with the initial state provided as last parameter).
-- Boot will return once a `Halt` EventResult has been processed in the event handler.
boot ::
  (Show rb, Ord rb, Ord rc, Ord n, MonadIO m, MonadError (RoguiError rc rb) m) =>
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
  let baseConsole = Console {width = w, height = h, position = SDL.V2 0 0}
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
            timerStep = timerStep,
            lastStep = 0,
            numberOfSteps = 0,
            targetFrameTime = frameTime,
            extentsMap = mempty
          }
  gui <- guiBuilder baseRogui

  appLoop gui initialState

  SDL.destroyWindow window

-- | Utility to look for a given brush in a map of brushes.
-- This will later be patched to handle error more gracefully.
brushLookup :: (Ord rb, Show rb) => M.Map rb Brush -> rb -> Brush
brushLookup m ref =
  fromMaybe (error $ "Brush not found: " ++ show ref) (M.lookup ref m)

appLoop :: (Show rb, Ord rb, Ord rc, Ord n, MonadIO m) => Rogui rc rb n s e -> s -> m ()
appLoop roGUI@Rogui {..} state = do
  sdlEvents <- getSDLEvents defaultBrush
  frameStart <- SDL.ticks
  let reachedStep = frameStart - lastStep > timerStep
      baseEvents = if reachedStep then Step : sdlEvents else sdlEvents
      baseEventState = EventHandlingState {events = Seq.fromList baseEvents, currentState = state, result = ContinueNoRedraw, knownExtents = extentsMap}
      EventHandlingState {result, currentState} = execState (processWithLimit 30 roGUI) baseEventState
  newExtents <-
    if (result == Continue)
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
    let elapsed = frameStart - frameEnd
        sleepMs =
          if elapsed < targetFrameTime
            then targetFrameTime - elapsed
            else 0
    liftIO $ threadDelay (fromIntegral sleepMs * 1000)
    let newRogui =
          roGUI
            { lastTicks = frameStart,
              lastStep = if reachedStep then frameStart else lastStep,
              numberOfSteps = if reachedStep then numberOfSteps + 1 else numberOfSteps,
              extentsMap = newExtents
            }
    appLoop newRogui currentState

processWithLimit :: Int -> Rogui rc rb n s e -> EventHandlingM s e n ()
processWithLimit 0 _ = pure ()
processWithLimit n roGUI = do
  evs <- gets events
  if null evs
    then pure ()
    else traverse (processEvent roGUI) evs >> processWithLimit (n - 1) roGUI

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
  onEvent currentState event

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
  EventHandler state e n ->
  EventHandler state e n
baseEventHandler sink state event =
  let ctrlC e = SDL.keysymKeycode e == SDL.KeycodeC && (SDL.keyModifierLeftCtrl . SDL.keysymModifier $ e)
   in case event of
        KeyDown KeyDownDetails {key} -> if ctrlC key then halt (pure ()) else sink state event
        OtherSDLEvent SDL.QuitEvent -> halt (pure ())
        Quit -> halt (pure ())
        OtherSDLEvent (SDL.WindowShownEvent _) -> redraw (pure ())
        Step -> redraw (pure ())
        _ -> sink state event

-- | A utility to react to key presses listed in a Map
keyPressHandler ::
  -- | Sink for events that have not been processed
  EventHandler state e n ->
  -- | A map of expected key codes and the actions to perform if this key was pressed
  M.Map SDL.Keycode (EventHandler state e n) ->
  EventHandler state e n
keyPressHandler sink keyMap state event =
  case event of
    KeyDown KeyDownDetails {key} ->
      let handler = (SDL.keysymKeycode key) `M.lookup` keyMap
       in maybe (sink state event) (\h -> h state event) handler
    _ -> sink state event

getSDLEvents :: (MonadIO m) => Brush -> m [Event e]
getSDLEvents Brush {..} =
  let toRoguiEvent (SDL.Event _timestamp payload) = case payload of
        SDL.KeyboardEvent ke -> case (SDL.keyboardEventKeyMotion ke) of
          SDL.Pressed -> KeyDown $ KeyDownDetails (SDL.keyboardEventRepeat ke) (SDL.keyboardEventKeysym ke)
          SDL.Released -> KeyUp . KeyUpDetails $ SDL.keyboardEventKeysym ke
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
