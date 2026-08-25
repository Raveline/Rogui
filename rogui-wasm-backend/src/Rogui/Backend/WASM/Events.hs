{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}

-- | Drains the JS-side event queue (populated by listeners installed by
-- `Rogui.Backend.WASM.initWASMBackend` via `RoguiRuntime.installListeners`,
-- see `jsbits/rogui-runtime.js`) and converts browser events into
-- `Rogui.Application.Event.Event`, mirroring
-- `Rogui.Backend.Events.getSDLEvents`.
--
-- One thing this mirrors deliberately: `getSDLEvents` deduplicates via
-- @S.toList . S.fromList@ over the *raw* `SDL.EventPayload`s before
-- converting to `Event e` (see `Rogui.Backend.Events`). That matters more
-- than it looks: a held key fires repeated, structurally-identical
-- `KeyDown` events, and `Rogui.Application.System.appTick` drains and
-- processes an entire poll's worth of events in one go -- so without
-- deduplication, several repeats of the same key landing in one tick can
-- walk a `wrapAround` list selection right past the end and back to the
-- start, which looks like the selection randomly jumping backwards. `Event
-- e` can't be deduplicated directly here the way SDL's code does it,
-- though: this backend (like the SDL one) is polymorphic in the host
-- app's custom event type `e`, which carries no `Eq`/`Ord`. So this reads
-- each queued browser event into a `RawEvent` (a plain, backend-internal,
-- `e`-free snapshot -- the equivalent of `SDL.EventPayload`), deduplicates
-- *that*, and only then converts the survivors to `Event e`.
module Rogui.Backend.WASM.Events
  ( pollWASMEvents,
    browserKeyToRoguiKey,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Char (isDigit)
import Data.List (stripPrefix)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Set qualified as S
import GHC.Wasm.Prim (JSVal)
import Linear (V2 (..))
import Rogui.Application.Event
  ( Event (..),
    Key (..),
    KeyDetails (..),
    KeyDownDetails (..),
    Modifier (..),
    MouseButton (..),
    MouseClickDetails (..),
    MouseEventDetails (..),
    MouseMoveDetails (..),
  )
import Rogui.Backend.WASM.FFI
import Rogui.Graphics.Types (Brush (..), Pixel (..), (./.=))

-- | A plain, `e`-free snapshot of one queued browser event -- every field
-- `RoguiRuntime`'s event objects can carry, always read in full regardless
-- of `rawKind` (an unused field for a given kind, e.g. `rawButton` for a
-- `keydown`, is always read as the same default), so that two genuinely
-- identical browser events (e.g. two key-repeat firings of the same key)
-- always compare equal here.
data RawEvent = RawEvent
  { rawKind :: Int,
    rawKey :: String,
    rawRepeat :: Bool,
    rawShift :: Bool,
    rawCtrl :: Bool,
    rawAlt :: Bool,
    rawX :: Int,
    rawY :: Int,
    rawDX :: Int,
    rawDY :: Int,
    rawButton :: Int,
    rawW :: Int,
    rawH :: Int
  }
  deriving (Eq, Ord)

readRawEvent :: JSVal -> IO RawEvent
readRawEvent ev = do
  rawKind <- js_eventKind ev
  rawKey <- jsValToString =<< js_eventKey ev
  rawRepeat <- js_eventRepeat ev
  rawShift <- js_eventShift ev
  rawCtrl <- js_eventCtrl ev
  rawAlt <- js_eventAlt ev
  rawX <- js_eventX ev
  rawY <- js_eventY ev
  rawDX <- js_eventDX ev
  rawDY <- js_eventDY ev
  rawButton <- js_eventButton ev
  rawW <- js_eventW ev
  rawH <- js_eventH ev
  pure RawEvent {..}

pollWASMEvents :: (MonadIO m) => Brush -> m [Event e]
pollWASMEvents Brush {..} = liftIO $ mapMaybe convertRaw . deduplicated <$> drain
  where
    drain = do
      ev <- js_popEvent
      isEmpty <- js_isNullOrUndefined ev
      if isEmpty
        then pure []
        else do
          raw <- readRawEvent ev
          rest <- drain
          pure (raw : rest)

    deduplicated = S.toList . S.fromList

    -- Kind codes are assigned by RoguiRuntime's event listeners; see
    -- js_eventKind's haddock in Rogui.Backend.WASM.FFI.
    convertRaw :: RawEvent -> Maybe (Event e)
    convertRaw RawEvent {..} = case rawKind of
      0 {- keydown -} ->
        Just $ KeyDown (KeyDownDetails rawRepeat (KeyDetails (browserKeyToRoguiKey rawKey) mods))
      1 {- keyup -} ->
        Just $ KeyUp (KeyDetails (browserKeyToRoguiKey rawKey) mods)
      2 {- mousemove -} ->
        let relativeMouseMotion = V2 (Pixel rawDX) (Pixel rawDY)
            absoluteMousePosition = absPos
            defaultTileSizePosition = cellPos
         in Just . MouseEvent . MouseMove $ MouseMoveDetails {..}
      3 {- mousedown -} -> Just . MouseEvent . MouseClickPressed $ MouseClickDetails absPos cellPos buttonClicked
      4 {- mouseup -} -> Just . MouseEvent . MouseClickReleased $ MouseClickDetails absPos cellPos buttonClicked
      5 {- resize -} -> Just $ WindowResized (V2 (Pixel rawW ./.= tileWidth) (Pixel rawH ./.= tileHeight))
      _ -> Nothing
      where
        mods =
          S.fromList . catMaybes $
            [ if rawShift then Just Shift else Nothing,
              if rawCtrl then Just Ctrl else Nothing,
              if rawAlt then Just Alt else Nothing
            ]
        absPos = V2 (Pixel rawX) (Pixel rawY)
        cellPos = V2 (Pixel rawX ./.= tileWidth) (Pixel rawY ./.= tileHeight)
        buttonClicked = case rawButton of
          0 -> LeftButton
          2 -> RightButton
          _ -> MiddleButton

-- | Map a `KeyboardEvent.key` string to a Rogui `Key`. Numpad digits are
-- not distinguished from top-row digits: the browser only reports that via
-- `code` (e.g. `"Numpad1"` vs `"Digit1"`), and `code` is layout-independent
-- in a way that would make `KChar` mappings wrong for non-QWERTY
-- keyboards, so `key` is used throughout and numpad digits fall through to
-- `KChar`.
browserKeyToRoguiKey :: String -> Key
browserKeyToRoguiKey k = case k of
  "Escape" -> KEsc
  "Enter" -> KEnter
  "ArrowLeft" -> KLeft
  "ArrowRight" -> KRight
  "ArrowUp" -> KUp
  "ArrowDown" -> KDown
  "Tab" -> KTab
  "Backspace" -> KBackspace
  "Home" -> KHome
  "End" -> KEnd
  "PageUp" -> KPageUp
  "PageDown" -> KPageDown
  "Delete" -> KDel
  "Insert" -> KIns
  "Pause" -> KPause
  "PrintScreen" -> KPrtScr
  _
    | Just digits <- stripPrefix "F" k,
      not (null digits),
      all isDigit digits ->
        KFun (read digits)
    | [c] <- k -> KChar c
    | otherwise -> KUnknown
