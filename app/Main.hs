{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (when)
import Control.Monad.Except
import Control.Monad.Writer
import Data.Array.IArray (Array, genArray, (!))
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes)
import Data.Set qualified as S
import Rogui.Application.Error (RoguiError)
import Rogui.Application.Event
import Rogui.Application.System
import Rogui.Application.Types (RoguiConfig (..))
import Rogui.Components.Button (button, handleButtonEvent)
import Rogui.Components.Core
import Rogui.Components.Game.EntitiesLayer (entitiesLayer)
import Rogui.Components.Game.GridTile
import Rogui.Components.Game.Utils (GlyphInfo (..))
import Rogui.Components.Label
import Rogui.Components.List hiding (scrollOffset)
import Rogui.Components.MessageLog (LogMessage, messageLog)
import Rogui.Components.ProgressBar
import Rogui.Components.TextInput
import Rogui.Components.Types
import Rogui.Components.Viewport (ViewportState (..), handleViewportEvent, viewport)
import Rogui.ConsoleSpecs
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
            stepMs = 100,
            eventFunction = baseEventHandler <||> eventHandler
          }
  bootAndPrintError
    config
    guiMaker
    $ State
      { gameState = PlayingGame,
        textValue = "test",
        listOfText = longListOfText,
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

guiMaker ::
  (MonadIO m, MonadError (RoguiError Consoles Brushes) m) =>
  Rogui Consoles Brushes Name State CustomEvent ->
  m (Rogui Consoles Brushes Name State CustomEvent)
guiMaker baseGui = do
  addBrush Charset "terminal_10x16.png" (TileSize 10 16) baseGui
    >>= addBrush BigCharset "terminal_16x16.png" (TileSize 16 16)
    >>= addConsoleWithSpec StatusBar (TilesSize 100 1) TopLeft
    >>= addConsoleWithSpec GameArea (SizeWindowPct 100 98) (Below StatusBar)
    >>= addConsoleWithSpec ModalMenu (TilesSize 40 25) Center
    >>= addConsoleWithSpec Logging FullWindow TopLeft

uiKeysHandler :: M.Map (SDL.Keycode, S.Set Modifier) (EventHandler State CustomEvent Name)
uiKeysHandler =
  M.fromList $
    [ ((SDL.KeycodeTab, []), \_ _ -> fireEvent FocusNext),
      ((SDL.KeycodeEscape, []), \_ _ -> fireAppEvent ToggleUI)
    ]

gameKeysHandler :: M.Map (SDL.Keycode, S.Set Modifier) (EventHandler State CustomEvent Name)
gameKeysHandler =
  M.fromList $
    [ ((SDL.KeycodeUp, []), \_ _ -> fireAppEvent . Move $ V2 0 (-1)),
      ((SDL.KeycodeDown, []), \_ _ -> fireAppEvent . Move $ V2 0 1),
      ((SDL.KeycodeLeft, []), \_ _ -> fireAppEvent . Move $ V2 (-1) 0),
      ((SDL.KeycodeRight, []), \_ _ -> fireAppEvent . Move $ V2 1 0),
      ((SDL.KeycodeF1, []), \_ _ -> fireAppEvent $ ToggleUI),
      ((SDL.KeycodeF2, []), \_ _ -> fireAppEvent $ ToggleLogs)
    ]

eventHandler :: EventHandler State CustomEvent Name
eventHandler state@State {..} e =
  case gameState of
    PlayingGame -> (keyPressHandler gameKeysHandler <||> gameEventHandler) state e
    UI -> (keyPressHandler uiKeysHandler <||> uiEventHandler) state e
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
  _ -> unhandled

logEventHandler :: EventHandler State CustomEvent Name
logEventHandler =
  let keyHandler = M.fromList [((SDL.KeycodeEscape, []), \_ _ -> fireAppEvent $ ToggleUI)]
      eventHandler' s = \case
        (AppEvent ToggleUI) -> modifyState $ \s' -> s' {gameState = PlayingGame}
        e -> handleViewportEvent MessageLogs e (logViewport s) $ \newViewport s' -> s' {logViewport = newViewport}
   in (keyPressHandler keyHandler <||> eventHandler')

uiEventHandler :: EventHandler State CustomEvent Name
uiEventHandler state@State {..} = \case
  -- Click events take priority and override focus
  MouseEvent (MouseClick c) -> handleClickEvent state c
  MouseEvent (MouseMove MouseMoveDetails {..}) ->
    redraw $ setCurrentState $ state {mousePosition = defaultTileSizePosition}
  FocusNext -> handleFocusChange (focusNext) state
  FocusPrev -> handleFocusChange (focusPrev) state
  (AppEvent ToggleUI) -> modifyState $ \s -> s {gameState = PlayingGame}
  e -> case focusGetCurrent ring of
    (Just List) -> handleLabelListEvent List listOfText False e listState (\newLs s -> s {listState = newLs})
    (Just QuitButton) -> handleButtonEvent (Quit) state e
    (Just TextInput) -> handleTextInputEvent e someText (\newString s -> s {someText = newString})
    _ -> unhandled

handleClickEvent :: ClickHandler State CustomEvent Name ()
handleClickEvent State {..} mc = do
  clicked <- foundClickedExtents mc
  when (QuitButton `elem` clicked) $ fireEvent Quit
  when (List `elem` clicked) $ handleClickOnLabelList List listOfText listState (\newLs s -> s {listState = newLs}) mc

handleFocusChange :: (FocusRing Name -> FocusRing Name) -> State -> EventHandlerM State CustomEvent Name ()
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
      fullMapSize = (V2 100 100)
      statusBar =
        hBox
          [ progressBar 0 20 10 baseColours baseColours fullBlock lightShade
          ]
      gameArea =
        multiLayeredGrid fullMapSize playerPos $
          [ gridTile ((!) arbitraryMap) tileToGlyphInfo,
            switchBrush bigCharset . entitiesLayer ([playerPos] :: [V2 Cell]) (const $ GlyphInfo 1 charColours) id
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
              bordered baseColours $ padded 2 $ labelList List listOfText id TLeft baseColours highlighted listState,
              padded 2 $ textInput TextInput someText textColours (focusGetCurrent ring == Just TextInput),
              vSize (Fixed 1) $
                button
                  QuitButton
                  "Quit"
                  TCenter
                  baseColours
                  highlighted
                  (focusGetCurrent ring == Just QuitButton)
            ]

renderLogging :: State -> Component Name
renderLogging State {logViewport} =
  filled black $ bordered (Colours (Just white) (Just black)) $ vBox [viewport MessageLogs (scrollOffset logViewport) $ messageLog fakeLogs]

-- Intentionally long to trigger scrolling behaviour
longListOfText :: [String]
longListOfText =
  [ "Abricot",
    "Banana",
    "Carrot",
    "Dewberry",
    "Eggplant",
    "Fig",
    "Grapes",
    "Honeydew",
    "Indian parsley",
    "Jalapeno",
    "Kiwi",
    "Lemon",
    "Mushroom",
    "Nectarine",
    "Orange",
    "Pear",
    "Quince",
    "Raspberry",
    "Strawberries",
    "Tomato"
  ]