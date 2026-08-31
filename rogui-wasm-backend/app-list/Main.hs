{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | A second WASM backend demo, this one interactive: a straight port of
-- `rogui-demos`' `ListDemo` (keyboard-navigable, clickable list, resizable
-- window) onto `wasmBackend`. Exercises the input/resize path the first
-- demo (`app/Main.hs`) doesn't: keyboard focus, mouse clicks against
-- recorded extents, and `allowResize`.
--
-- Structurally this is identical to `app/Main.hs` -- same `Rogui.Backend.WASM.Run`
-- shim, same `-no-hs-main` + `app/cbits/wasm_main.c` (shared, not duplicated;
-- see the cabal file) -- only `DemoState`/`RoguiConfig`/the drawing and event
-- functions differ, ported verbatim from the SDL original.
module Main (main) where

import Control.Monad (when)
import Control.Monad.Except (ExceptT, runExceptT)
import Data.Map qualified as M
import Linear (V2 (..))
import Log (LogT)
import Rogui.Application
import Rogui.Backend.WASM (wasmBackend)
import Rogui.Backend.WASM.Run (WasmApp (..), mkWasmApp)
import Rogui.Components.Core (Component (..), bordered, emptyComponent, vBox)
import Rogui.Components.List
import Rogui.Graphics
import System.IO.Unsafe (unsafePerformIO)

data Consoles = Root
  deriving (Show, Eq, Ord)

data Brushes = Charset
  deriving (Show, Eq, Ord)

data Names = DemoList
  deriving (Show, Eq, Ord)

newtype DemoState = DemoState {listState :: ListState}

type AppM = ExceptT (RoguiError () Consoles Brushes) (LogT IO)

{-# NOINLINE wasmApp #-}
wasmApp :: WasmApp
wasmApp =
  unsafePerformIO $
    mkWasmApp
      wasmBackend
      (withoutLogging . runExceptT)
      config
      DemoState {listState = mkListState}

foreign export javascript "wasmInit" hsWasmInit :: IO ()

hsWasmInit :: IO ()
hsWasmInit = wasmAppInit wasmApp

foreign export javascript "wasmTick" hsWasmTick :: IO Bool

hsWasmTick :: IO Bool
hsWasmTick = wasmAppTick wasmApp

main :: IO ()
main = pure ()

config :: RoguiConfig Consoles Brushes Names DemoState () AppM
config =
  RoguiConfig
    { brushTilesize = TileSize 10 16,
      appName = "RoGUI WASM list demo",
      consoleCellSize = V2 80 38,
      targetFPS = 60,
      rootConsoleReference = Root,
      defaultBrushReference = Charset,
      defaultBrushPath = Right "terminal_10x16.png",
      defaultBrushTransparencyColour = pure black,
      drawingFunction = renderApp,
      stepMs = 100,
      eventFunction = baseEventHandler <||> handleEvent,
      consoleSpecs = [],
      brushesSpecs = [],
      allowResize = True,
      maxEventDepth = 100
    }

-- | Before you come yelling at me with Berlin interpretation:
-- this is just stupid lorem ipsum for the demo
data Item = Rogue | Nethack | Angband | Larn | ADOM | DCSS | Diablo | Elona | Cogmind | Balatro | CaveOfQud | DwarfFortress | DoomRL | DungeonsOfDredmor
  deriving (Eq, Show, Enum, Bounded)

allItems :: [Item]
allItems = [Rogue .. maxBound]

listDefinition :: ListDefinition Names Item
listDefinition = ListDefinition {name = DemoList, items = allItems, renderItem = manyLineDescription, itemHeight = 3, wrapAround = True}

handleEvent :: (Monad m) => EventHandler m DemoState () Names
handleEvent ds@DemoState {..} = \case
  (MouseEvent (MouseClickReleased mcd)) -> handleClickEvent ds mcd
  e -> handleListEvent listDefinition listState (\ls s' -> s' {listState = ls}) ds e

handleClickEvent :: (Monad m) => ClickHandler m DemoState () Names ()
handleClickEvent DemoState {..} mc = do
  clicked <- foundClickedExtents mc
  when (DemoList `elem` clicked) $ handleClickOnList listDefinition Nothing listState (\ls s' -> s' {listState = ls}) mc

manyLineDescription :: Item -> Bool -> Component n
manyLineDescription item focused =
  let titleColour = Colours (Just red) (Just black)
      colours = Colours (Just white) (Just black)
      description = case item of
        Rogue -> "The great ancestor, creator of the Genre, inventor of the Amulet of Yendor"
        Nethack -> "A fork of a fork, from whom many forked in their turn"
        Angband -> "A new challenger, who pushed Umoria further."
        ADOM -> "The one with a real story, and lots of various environment"
        DCSS -> "The one that is supposed to be FAIR."
        Larn -> "The one that doesn't take you hours to finish"
        Diablo -> "The one that added more slash to the hack"
        Elona -> "The one where you can do way too many stuff"
        Cogmind -> "The modern classic by excellence"
        Balatro -> "The one with... poker ?"
        CaveOfQud -> "The one with sentient plants"
        DwarfFortress -> "The sacred monster."
        DoomRL -> "The one that looked dumb but actually wasn't at all."
        DungeonsOfDredmor -> "The one with good music"
      draw' = do
        when focused (setConsoleBackground white)
        setColours $ if focused then invert titleColour else titleColour
        strLn TLeft (show item)
        strLn TLeft ""
        setColours $ if focused then invert colours else colours
        strLn TLeft description
   in emptyComponent {draw = draw'}

renderApp :: M.Map Brushes Brush -> DemoState -> [(Maybe Consoles, Maybe Brushes, Component Names)]
renderApp _ DemoState {..} =
  let content =
        bordered (Colours (Just white) (Just black))
          . vBox
          $ [ list listDefinition listState
            ]
   in [(Nothing, Nothing, content)]
