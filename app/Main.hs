{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (when)
import Control.Monad.Writer
import Data.Array.IArray (Array, genArray, (!))
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes)
import Rogui.Application.Event
import Rogui.Application.System
import Rogui.Application.Types (RoguiConfig (..))
import Rogui.Components.Button (button, handleButtonEvent)
import Rogui.Components.Core
import Rogui.Components.Game.EntitiesLayer (entitiesLayer)
import Rogui.Components.Game.GridTile
import Rogui.Components.Game.Utils (GlyphInfo (..), computeMapViewport)
import Rogui.Components.Label
import Rogui.Components.List hiding (scrollOffset)
import Rogui.Components.MessageLog (LogMessage, messageLog)
import Rogui.Components.ProgressBar
import Rogui.Components.TextInput
import Rogui.Components.Types
import Rogui.Components.Viewport (ViewportState (..), handleViewportEvent, viewport)
import Rogui.FocusRing
import Rogui.Graphics
import Rogui.Types
import SDL hiding (Event, drawLine, textureHeight, textureWidth)

data Consoles = Root | ModalMenu | StatusBar | GameArea | Logging
  deriving (Eq, Ord, Show)

data Brushes = Charset | BigCharset | Drawings
  deriving (Eq, Ord, Show)

data State = State
  { gameState :: GameState,
    textValue :: String,
    listOfText :: [String],
    mousePosition :: V2 Cell,
    ring :: FocusRing Name,
    listState :: ListState,
    someText :: String,
    playerPos :: V2 Cell,
    logViewport :: ViewportState
  }

data GameState = PlayingGame | UI | LogView
  deriving (Eq)

data TileType
  = Floor
  | Wall

tileToGlyphId :: TileType -> Int
tileToGlyphId = \case
  Floor -> 5
  Wall -> 78

tileToGlyphInfo :: TileType -> GlyphInfo
tileToGlyphInfo t =
  GlyphInfo
    { colours = Colours Nothing Nothing,
      glyphId = tileToGlyphId t
    }

arbitraryMap :: Array (V2 Cell) TileType
arbitraryMap =
  let generator (V2 x y) =
        if (x `mod` 3 == 0) && (y `mod` 3 == 0) then Wall else Floor
   in genArray ((V2 0 0), (V2 100 100)) generator

data Name
  = List
  | TextInput
  | QuitButton
  | MessageLogs
  deriving (Eq, Ord)

data CustomEvent = Move (V2 Cell) | ToggleUI | ToggleLogs

main :: IO ()
main = do
  let config =
        RoguiConfig
          { brushTilesize = TileSize 16 16,
            appName = "RoGUI example",
            consoleCellSize = (V2 50 38),
            targetFPS = 60,
            rootConsoleReference = Root,
            defaultBrushReference = Drawings,
            defaultBrushPath = "punyworld-dungeon-tileset.png",
            drawingFunction = renderApp,
            eventFunction = baseEventHandler eventHandler
          }
  boot
    config
    guiMaker
    $ State
      { gameState = PlayingGame,
        textValue = "test",
        listOfText = ["Item 1", "Item 2", "Item 3"],
        mousePosition = V2 0 0,
        ring = focusRing [List, TextInput, QuitButton],
        listState = mkListState {selection = Just 0},
        someText = "",
        playerPos = V2 1 1,
        logViewport = ViewportState (V2 0 0) (V2 0 (Cell $ length fakeLogs))
      }

fakeLogs :: [LogMessage]
fakeLogs =
  let basicLogs =
        [ [(Colours (Just white) (Just black), "This is a log message")],
          [(Colours (Just white) (Just black), "This is another log message")],
          [(Colours (Just white) (Just black), "And another one")]
        ]
   in mconcat $ replicate 200 basicLogs

guiMaker :: (MonadIO m) => Rogui Consoles Brushes Name State CustomEvent -> m (Rogui Consoles Brushes Name State CustomEvent)
guiMaker baseGui = do
  let modal = Console {width = 400, height = 400, position = V2 (16 * 10) (16 * 10)}
      statusBar = Console {width = 800, height = 16, position = V2 0 0}
      gameArea = Console {width = 800, height = 784, position = V2 0 16}
      loggingConsole = Console {width = 800, height = 608, position = V2 0 0}
  withBrushes <-
    addBrush Charset "terminal_10x16.png" (TileSize 10 16) baseGui
      >>= addBrush BigCharset "terminal_16x16.png" (TileSize 16 16)
  pure
    . addConsole ModalMenu modal
    . addConsole StatusBar statusBar
    . addConsole GameArea gameArea
    . addConsole Logging loggingConsole
    $ withBrushes

uiKeysHandler :: M.Map SDL.Keycode (EventHandler State CustomEvent Name)
uiKeysHandler =
  M.fromList $
    [ (SDL.KeycodeTab, \_ _ -> fireEvent FocusNext),
      (SDL.KeycodeEscape, \_ _ -> fireAppEvent ToggleUI)
    ]

gameKeysHandler :: M.Map SDL.Keycode (EventHandler State CustomEvent Name)
gameKeysHandler =
  M.fromList $
    [ (SDL.KeycodeUp, \_ _ -> fireAppEvent . Move $ V2 0 (-1)),
      (SDL.KeycodeDown, \_ _ -> fireAppEvent . Move $ V2 0 1),
      (SDL.KeycodeLeft, \_ _ -> fireAppEvent . Move $ V2 (-1) 0),
      (SDL.KeycodeRight, \_ _ -> fireAppEvent . Move $ V2 1 0),
      (SDL.KeycodeF1, \_ _ -> fireAppEvent $ ToggleUI),
      (SDL.KeycodeF2, \_ _ -> fireAppEvent $ ToggleLogs)
    ]

eventHandler :: EventHandler State CustomEvent Name
eventHandler state@State {..} e =
  case gameState of
    PlayingGame -> keyPressHandler gameEventHandler gameKeysHandler state e
    UI -> keyPressHandler uiEventHandler uiKeysHandler state e
    LogView -> logEventHandler state e

gameEventHandler :: EventHandler State CustomEvent Name
gameEventHandler State {..} = \case
  (AppEvent (Move dir)) -> do
    let newPos@(V2 x y) = playerPos + dir
    when (x >= 0 && y >= 0 && x <= 100 && y <= 100) $
      modifyState $
        \s -> s {playerPos = newPos}
  (AppEvent ToggleUI) -> modifyState $ \s -> s {gameState = UI}
  (AppEvent ToggleLogs) -> modifyState $ \s -> s {gameState = LogView}
  _ -> pure ()

logEventHandler :: EventHandler State CustomEvent Name
logEventHandler =
  let keyHandler = M.fromList [(SDL.KeycodeEscape, \_ _ -> fireAppEvent $ ToggleUI)]
      eventHandler' s = \case
        (AppEvent ToggleUI) -> modifyState $ \s' -> s' {gameState = PlayingGame}
        e -> handleViewportEvent MessageLogs e (logViewport s) $ \newViewport s' -> s' {logViewport = newViewport}
   in keyPressHandler eventHandler' keyHandler

uiEventHandler :: EventHandler State CustomEvent Name
uiEventHandler state@State {..} = \case
  MouseEvent (MouseMove MouseMoveDetails {..}) ->
    redraw $ setCurrentState $ state {mousePosition = defaultTileSizePosition}
  FocusNext -> handleFocusChange (focusNext) state
  FocusPrev -> handleFocusChange (focusPrev) state
  (AppEvent ToggleUI) -> modifyState $ \s -> s {gameState = PlayingGame}
  e -> case focusGetCurrent ring of
    (Just List) -> handleListEvent (length listOfText) e listState (\newLs s -> s {listState = newLs})
    (Just QuitButton) -> handleButtonEvent (\_ _ -> halt (pure ())) state e
    (Just TextInput) -> handleTextInputEvent e someText (\newString s -> s {someText = newString})
    _ -> pure ()

handleFocusChange :: (FocusRing Name -> FocusRing Name) -> State -> EventHandlingM State CustomEvent Name ()
handleFocusChange ringChange s =
  let newRing = ringChange (ring s)
      -- When focus moves to List and selection is Nothing, initialize
      newListState = case (focusGetCurrent newRing, listState s) of
        (Just List, ListState Nothing offset) -> ListState (Just 0) offset
        _ -> listState s
   in redraw (setCurrentState $ s {ring = newRing, listState = newListState})

renderApp :: M.Map Brushes Brush -> State -> ToDraw Consoles Brushes Name
renderApp brushes s@State {playerPos, gameState} =
  let baseColours = Colours (Just white) (Just black)
      charColours = Colours (Just white) Nothing
      bigCharset = brushes M.! BigCharset
      gridTileSize = (V2 40 30)
      fullMapSize = (V2 100 100)
      viewport' = computeMapViewport gridTileSize fullMapSize playerPos
      statusBar =
        hBox
          [ progressBar 0 20 10 baseColours baseColours fullBlock lightShade
          ]
      gameArea =
        vBox
          [ multiLayeredGrid fullMapSize viewport' $
              [ gridTile gridTileSize ((!) arbitraryMap) tileToGlyphInfo,
                switchBrush bigCharset . entitiesLayer [playerPos] (const $ GlyphInfo 1 charColours) id
              ]
          ]
   in catMaybes $
        [ Just (Just StatusBar, Just Charset, statusBar),
          Just (Just GameArea, Just Drawings, gameArea),
          if gameState == UI then Just (Just ModalMenu, Just Charset, renderUI s) else Nothing,
          if gameState == LogView then Just (Just Logging, Just Charset, renderLogging s) else Nothing
        ]

renderUI :: State -> Component Name
renderUI State {..} =
  let baseColours = Colours (Just white) (Just black)
      highlighted = Colours (Just black) (Just white)
      textColours = Colours (Just white) (Just grey)
      (V2 x y) = mousePosition
   in filled black $
        bordered baseColours $
          vBox
            [ vSize (Fixed 1) $
                label
                  ("Mouse at: " <> show (x, y))
                  TRight
                  baseColours,
              vSize (Fixed 2) $
                ( label
                    textValue
                    TCenter
                    ( Colours
                        (Just red)
                        (Just black)
                    )
                ),
              bordered baseColours $ padded 2 $ list listOfText id TLeft baseColours highlighted listState,
              padded 2 $ textInput someText textColours (focusGetCurrent ring == Just TextInput),
              vSize (Fixed 1) $
                button
                  "Quit"
                  TCenter
                  baseColours
                  highlighted
                  (focusGetCurrent ring == Just QuitButton)
            ]

renderLogging :: State -> Component Name
renderLogging State {logViewport} =
  filled black $ bordered (Colours (Just white) (Just black)) $ vBox [viewport MessageLogs (scrollOffset logViewport) $ messageLog fakeLogs]