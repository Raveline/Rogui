{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}

module Rogui.Application.System
  ( loadBrush,
    boot,
    baseEventHandler,
    keyPressHandler,
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State hiding (state)
import Data.Map qualified as M
import Data.Sequence qualified as Seq
import Data.Text hiding (foldl', null)
import Data.Word
import Rogui.Application.Event
import Rogui.Components (renderComponents)
import Rogui.Graphics.Types (Brush (..), Cell, Console (..), Pixel, TileSize (..), (.*=.), (./.=))
import Rogui.Types (EventHandler, Rogui (..))
import SDL (MouseMotionEventData (MouseMotionEventData))
import SDL qualified
import SDL.Image qualified as SDL
import SDL.Internal.Numbered qualified as Numbered
import SDL.Raw qualified as Raw

convertSurface :: (MonadIO m) => SDL.Surface -> SDL.PixelFormat -> m SDL.Surface
convertSurface (SDL.Surface s _) pixFmt = do
  fmt <- Raw.allocFormat (Numbered.toNumber pixFmt)
  surface <- SDL.Surface <$> Raw.convertSurface s fmt 0 <*> pure Nothing
  surface <$ Raw.freeFormat fmt

loadBrush :: (MonadIO m) => SDL.Renderer -> FilePath -> SDL.V2 Pixel -> m Brush
loadBrush renderer path (SDL.V2 tileWidth tileHeight) = do
  fontSurface <- SDL.load path
  surface <- convertSurface fontSurface SDL.RGBA8888
  let black :: SDL.V4 Word8
      black = SDL.V4 0 0 0 0x00
  void $ SDL.surfaceColorKey surface SDL.$= pure black
  brush <- SDL.createTextureFromSurface renderer surface
  textInfo <- SDL.queryTexture brush
  pure $ Brush {tileWidth, tileHeight, textureWidth = fromIntegral (SDL.textureWidth textInfo), textureHeight = fromIntegral (SDL.textureHeight textInfo), brush}

-- Initialize a SDL application and window with the provided tilesize,
-- giving a window with a size expressed in tiles.
boot :: (MonadIO m) => TileSize -> Text -> SDL.V2 Cell -> Int -> (SDL.Renderer -> Console -> m (Rogui rc rb n s e)) -> s -> m ()
boot TileSize {..} title (SDL.V2 widthInTiles heightInTiles) fps guiBuilder initialState = do
  SDL.initializeAll

  let windowSize@(SDL.V2 w h) = SDL.V2 (pixelWidth .*=. widthInTiles) (pixelHeight .*=. heightInTiles)
  window <- SDL.createWindow title SDL.defaultWindow {SDL.windowInitialSize = fromIntegral <$> windowSize}
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  let baseConsole = Console {width = w, height = h, position = SDL.V2 0 0}
      frameTime = 1000 `div` fromIntegral fps -- milliseconds per frame
  gui <- guiBuilder renderer baseConsole

  appLoop gui {targetFrameTime = frameTime} initialState

  SDL.destroyWindow window

appLoop :: (MonadIO m) => Rogui rc rb n s e -> s -> m ()
appLoop roGUI@Rogui {..} state = do
  sdlEvents <- getSDLEvents defaultBrush
  frameStart <- SDL.ticks
  let reachedStep = frameStart - lastStep > timerStep
      baseEvents = if reachedStep then Step : sdlEvents else sdlEvents
      baseEventState = EventHandlingState {events = Seq.fromList baseEvents, currentState = state, result = ContinueNoRedraw}
      EventHandlingState {result, currentState} = execState (processWithLimit 10 roGUI) baseEventState
  when (result == Continue) $ do
    SDL.clear renderer
    let toDraw = draw currentState
    renderComponents roGUI toDraw
    SDL.present renderer
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
              numberOfSteps = if reachedStep then numberOfSteps + 1 else numberOfSteps
            }
    appLoop newRogui currentState

processWithLimit :: Int -> Rogui rc rb n s e -> EventHandlingM s e ()
processWithLimit 0 _ = pure ()
processWithLimit n roGUI = do
  evs <- gets events
  if null evs
    then pure ()
    else traverse (processEvent roGUI) evs >> processWithLimit (n - 1) roGUI

processEvent :: Rogui rc rb n s e -> Event e -> EventHandlingM s e ()
processEvent Rogui {..} event = do
  _ <- popEvent -- This will remove the event from the queue
  currentState <- gets currentState
  onEvent currentState event

-- | A default event handler that will:
--
-- - React to ALT+F4, clicking the window cross, or ctrl+C to quit the
-- application
-- - Ensure that a first render is done on window shown
-- - Ensure that rendering is done when Step is reached
--
-- Other events are to be manually implemented.  Feed your own event handler to
-- this so you get an easy way to leave your applications through common
-- shortcuts.
baseEventHandler :: EventHandler state e -> EventHandler state e
baseEventHandler sink state event =
  let ctrlC e = SDL.keysymKeycode e == SDL.KeycodeC && (SDL.keyModifierLeftCtrl . SDL.keysymModifier $ e)
   in case event of
        KeyDown KeyDownDetails {key} -> if ctrlC key then halt (pure ()) else sink state event
        OtherSDLEvent SDL.QuitEvent -> halt (pure ())
        OtherSDLEvent (SDL.WindowShownEvent _) -> redraw (pure ())
        Step -> redraw (pure ())
        _ -> sink state event

-- | A utility to react to key presses listed in a Map
keyPressHandler :: EventHandler state e -> M.Map SDL.Keycode (EventHandler state e) -> EventHandler state e
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
        SDL.MouseButtonEvent me -> MouseEvent (MouseClick me)
        e -> OtherSDLEvent e
   in fmap (fmap toRoguiEvent) SDL.pollEvents
