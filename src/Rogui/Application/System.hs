{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}

module Rogui.Application.System
  ( loadBrush,
    boot,
    baseEventHandler,
    keyPressHandler,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.Map qualified as M
import Data.Text hiding (foldl')
import Data.Word
import Rogui.Application.Event
import Rogui.Components (renderComponents)
import Rogui.Graphics.Types (Brush (..), Console (..), TileSize (..))
import Rogui.Types (Rogui (..))
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

loadBrush :: (MonadIO m) => SDL.Renderer -> FilePath -> SDL.V2 Int -> m Brush
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
boot :: (MonadIO m) => TileSize -> Text -> SDL.V2 Int -> (SDL.Renderer -> Console -> m (Rogui rc rb n s)) -> s -> m ()
boot TileSize {..} title (SDL.V2 widthInTiles heightInTiles) guiBuilder initialState = do
  SDL.initializeAll

  let windowSize@(SDL.V2 w h) = SDL.V2 (widthInTiles * pixelWidth) (heightInTiles * pixelHeight)
  window <- SDL.createWindow title SDL.defaultWindow {SDL.windowInitialSize = fromIntegral <$> windowSize}
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  let baseConsole = Console {width = w, height = h, position = SDL.V2 0 0}
  gui <- guiBuilder renderer baseConsole

  appLoop gui initialState

  SDL.destroyWindow window

appLoop :: (MonadIO m) => Rogui rc rb n s -> s -> m ()
appLoop roGUI@Rogui {..} state = do
  events <- getSDLEvents defaultBrush
  let (finalResult, updatedState) = foldl' (processEvent roGUI) (ContinueNoRedraw, state) events
  when (finalResult == Continue) $ do
    SDL.clear renderer
    let toDraw = draw updatedState
    renderComponents renderer rootConsole defaultBrush toDraw
    SDL.present renderer
  unless (finalResult == Halt) $
    appLoop roGUI updatedState

processEvent :: Rogui rc rb n s -> (EventResult, s) -> Event -> (EventResult, s)
processEvent Rogui {..} (currentResult, state) event =
  let (newResult, newState) = onEvent state event
   in (currentResult <> newResult, newState)

type EventHandler state = state -> Event -> (EventResult, state)

-- | A default event handler that will:
-- - React to ALT+F4, clicking the window cross, or ctrl+C to quit the application
-- - Ensure that a first render is done on window shown
-- Other events are to be manually
-- implemented. Feed your own event handler to this so you get an easy way to
-- leave your applications through common shortcuts.
baseEventHandler :: EventHandler state -> EventHandler state
baseEventHandler sink state event =
  let ctrlC e = SDL.keysymKeycode e == SDL.KeycodeC && (SDL.keyModifierLeftCtrl . SDL.keysymModifier $ e)
   in case event of
        KeyDown KeyDownDetails {key} -> if ctrlC key then (Halt, state) else sink state event
        OtherSDLEvent SDL.QuitEvent -> (Halt, state)
        OtherSDLEvent (SDL.WindowShownEvent _) -> (Continue, state)
        _ -> sink state event

-- | A utility to react to key presses listed in a Map
keyPressHandler :: EventHandler state -> M.Map SDL.Keycode (state -> (EventResult, state)) -> EventHandler state
keyPressHandler sink keyMap state event =
  case event of
    KeyDown KeyDownDetails {key} ->
      let handler = (SDL.keysymKeycode key) `M.lookup` keyMap
       in maybe (sink state event) (\h -> h state) handler
    _ -> sink state event

getSDLEvents :: (MonadIO m) => Brush -> m [Event]
getSDLEvents Brush {..} =
  let toRoguiEvent (SDL.Event _timestamp payload) = case payload of
        SDL.KeyboardEvent ke -> case (SDL.keyboardEventKeyMotion ke) of
          SDL.Pressed -> KeyDown $ KeyDownDetails (SDL.keyboardEventRepeat ke) (SDL.keyboardEventKeysym ke)
          SDL.Released -> KeyUp . KeyUpDetails $ SDL.keyboardEventKeysym ke
        SDL.MouseMotionEvent MouseMotionEventData {..} ->
          let (SDL.P mousePos) = mouseMotionEventPos
              absoluteMousePosition@(SDL.V2 x y) = fromIntegral <$> mousePos
              defaultTileSizePosition = SDL.V2 (x `div` tileWidth) (y `div` tileHeight)
              relativeMouseMotion = fromIntegral <$> mouseMotionEventRelMotion
           in MouseEvent . MouseMove $ MouseMoveDetails {..}
        SDL.MouseButtonEvent me -> MouseEvent (MouseClick me)
        e -> OtherSDLEvent e
   in fmap (fmap toRoguiEvent) SDL.pollEvents
