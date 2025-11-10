{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (when)
import Control.Monad.Except
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Writer
import Data.Array.IArray (Array, genArray, (!))
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes)
import Data.Set qualified as S
import Rogui.Application.Error (RoguiError)
import Rogui.Application.Event
import Rogui.Application.System
import Rogui.Components.Button (button, handleButtonEvent)
import Rogui.Components.Core
import Rogui.Components.Game.EntitiesLayer (entitiesLayer)
import Rogui.Components.Game.GridTile
import Rogui.Components.Game.Utils (GlyphInfo (..))
import Rogui.Components.Label
import Rogui.Components.List (FocusOrigin (..), ListState)
import Rogui.Components.List qualified as L
import Rogui.Components.MessageLog (LogMessage, messageLog)
import Rogui.Components.ProgressBar
import Rogui.Components.TextInput
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
    worldPosition :: Maybe (V2 Cell),
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
      glyphId = tileToGlyphId t,
      transformations = []
    }

arbitraryMap :: Array (V2 Cell) TileType
arbitraryMap =
  let generator (V2 x y) =
        if (x `mod` 3 == 0) && (y `mod` 3 == 0) then Wall else Floor
   in genArray (V2 0 0, V2 99 99) generator

data Name
  = List
  | TextInput
  | QuitButton
  | MessageLogs
  | GameGrid
  deriving (Eq, Ord)

data CustomEvent = Move (V2 Cell) | ToggleUI | ToggleLogs

main :: IO ()
main = do
  let config =
        RoguiConfig
          { brushTilesize = TileSize 16 16,
            appName = "RoGUI example",
            consoleCellSize = V2 50 38,
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
        worldPosition = Nothing,
        ring = focusRing [List, TextInput, QuitButton],
        listState = L.mkListState {L.selection = Just 0},
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
  (MonadIO m, MonadError (RoguiError Consoles Brushes) m, MonadLogger m) =>
  Rogui Consoles Brushes Name State CustomEvent m' ->
  m (Rogui Consoles Brushes Name State CustomEvent m')
guiMaker baseGui = do
  let ts10x16 = TileSize 10 16
      ts16x16 = TileSize 16 16
  addBrush Charset "terminal_10x16.png" ts10x16 baseGui
    >>= addBrush BigCharset "terminal_16x16.png" ts16x16
    >>= addConsoleWithSpec StatusBar ts10x16 (TilesSize 100 1) TopLeft
    >>= addConsoleWithSpec GameArea ts16x16 (SizeWindowPct 100 98) (Below StatusBar)
    >>= addConsoleWithSpec ModalMenu ts10x16 (TilesSize 40 25) Center
    >>= addConsoleWithSpec Logging ts10x16 FullWindow TopLeft

uiKeysHandler :: (Monad m) => M.Map (SDL.Keycode, S.Set Modifier) (EventHandler m State CustomEvent Name)
uiKeysHandler =
  M.fromList
    [ ((SDL.KeycodeTab, []), \_ _ -> fireEvent FocusNext),
      ((SDL.KeycodeEscape, []), \_ _ -> fireAppEvent ToggleUI)
    ]

gameKeysHandler :: (Monad m) => M.Map (SDL.Keycode, S.Set Modifier) (EventHandler m State CustomEvent Name)
gameKeysHandler =
  M.fromList
    [ ((SDL.KeycodeUp, []), \_ _ -> fireAppEvent . Move $ V2 0 (-1)),
      ((SDL.KeycodeDown, []), \_ _ -> fireAppEvent . Move $ V2 0 1),
      ((SDL.KeycodeLeft, []), \_ _ -> fireAppEvent . Move $ V2 (-1) 0),
      ((SDL.KeycodeRight, []), \_ _ -> fireAppEvent . Move $ V2 1 0),
      ((SDL.KeycodeF1, []), \_ _ -> fireAppEvent ToggleUI),
      ((SDL.KeycodeF2, []), \_ _ -> fireAppEvent ToggleLogs)
    ]

eventHandler :: (Monad m) => EventHandler m State CustomEvent Name
eventHandler state@State {..} e =
  case gameState of
    PlayingGame -> (keyPressHandler gameKeysHandler <||> gameEventHandler <||> gridMouseHandler) state e
    UI -> (keyPressHandler uiKeysHandler <||> uiEventHandler) state e
    LogView -> logEventHandler state e

gameEventHandler :: (Monad m) => EventHandler m State CustomEvent Name
gameEventHandler State {..} = \case
  (AppEvent (Move dir)) -> do
    let newPos@(V2 x y) = playerPos + dir
    when (x >= 0 && y >= 0 && x < 100 && y < 100) $
      modifyState $
        \s -> s {playerPos = newPos}
  (AppEvent ToggleUI) -> modifyState $ \s -> s {gameState = UI}
  (AppEvent ToggleLogs) -> modifyState $ \s -> s {gameState = LogView}
  _ -> unhandled

gridMouseHandler :: (Monad m) => EventHandler m State CustomEvent Name
gridMouseHandler State {..} = \case
  (MouseEvent (MouseClick MouseClickDetails {..})) -> do
    worldPos <- mouseEventToWorldPos GameGrid (TileSize 16 16) fullMapSize playerPos absoluteMousePosition
    redraw $ modifyState (\s -> s {worldPosition = worldPos})
  _ -> unhandled

logEventHandler :: (Monad m) => EventHandler m State CustomEvent Name
logEventHandler =
  let keyHandler = M.fromList [((SDL.KeycodeEscape, []), \_ _ -> fireAppEvent ToggleUI)]
      eventHandler' s = \case
        (AppEvent ToggleUI) -> modifyState $ \s' -> s' {gameState = PlayingGame}
        e -> handleViewportEvent MessageLogs e (logViewport s) $ \newViewport s' -> s' {logViewport = newViewport}
   in (keyPressHandler keyHandler <||> eventHandler')

uiEventHandler :: (Monad m) => EventHandler m State CustomEvent Name
uiEventHandler state@State {..} = \case
  -- Click events take priority and override focus
  MouseEvent (MouseClick c) -> handleClickEvent state c
  MouseEvent (MouseMove MouseMoveDetails {..}) ->
    redraw $ setCurrentState $ state {mousePosition = defaultTileSizePosition}
  FocusNext -> handleFocusChange FromNext state
  FocusPrev -> handleFocusChange FromPrev state
  (AppEvent ToggleUI) -> modifyState $ \s -> s {gameState = PlayingGame}
  e -> case focusGetCurrent ring of
    (Just List) -> L.handleLabelListEvent List listOfText False e listState (\newLs s -> s {listState = newLs})
    (Just QuitButton) -> handleButtonEvent Quit state e
    (Just TextInput) -> handleTextInputEvent e someText (\newString s -> s {someText = newString})
    _ -> unhandled

handleClickEvent :: (Monad m) => ClickHandler m State CustomEvent Name ()
handleClickEvent State {..} mc = do
  clicked <- foundClickedExtents mc
  when (QuitButton `elem` clicked) $ fireEvent Quit
  when (List `elem` clicked) $ L.handleClickOnLabelList List listOfText listState (\newLs s -> s {listState = newLs}) mc

handleFocusChange :: (Monad m) => FocusOrigin -> State -> EventHandlerM m State CustomEvent Name ()
handleFocusChange focusOrigin s = do
  let newRing = (if focusOrigin == FromNext then focusNext else focusPrev) (ring s)
  -- When focus moves to List, update its state appropriately
  when (focusGetCurrent newRing == Just List) $ do
    L.labelListReceiveFocus List (listOfText s) (listState s) focusOrigin (\newLs s' -> s' {listState = newLs})
  modifyState $ \s' -> s' {ring = newRing}

fullMapSize :: V2 Cell
fullMapSize = V2 100 100

renderApp :: M.Map Brushes Brush -> State -> ToDraw Consoles Brushes Name
renderApp brushes s@State {playerPos, gameState, worldPosition} =
  let baseColours = Colours (Just white) (Just black)
      charColours = Colours (Just white) Nothing
      bigCharset = brushes M.! BigCharset
      statusBarDefinition =
        ProgressBarDefinition
          { minimumValue = 0,
            maximumValue = 100,
            value = 100,
            coloursFilled = baseColours,
            coloursUnfilled = baseColours,
            glyphFilled = fullBlock,
            glyphUnfilled = lightShade
          }
      statusBar =
        hBox . catMaybes $
          [ Just $ hSize (Fixed 20) $ progressBar statusBarDefinition,
            (\wp -> Just $ label ("Clicked on " <> show wp) TLeft baseColours) =<< worldPosition
          ]
      gameArea =
        withRecordedExtent GameGrid $
          multiLayeredGrid
            fullMapSize
            playerPos
            [ gridTile (arbitraryMap !) tileToGlyphInfo,
              trySwitchBrush bigCharset . entitiesLayer ([playerPos] :: [V2 Cell]) (const $ GlyphInfo 1 charColours []) id
            ]
   in catMaybes
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
              vSize
                (Fixed 2)
                ( label
                    textValue
                    TCenter
                    ( Colours
                        (Just red)
                        (Just black)
                    )
                ),
              bordered baseColours $ padded 2 $ L.labelList List listOfText id TLeft baseColours highlighted listState,
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
  filled black $ bordered (Colours (Just white) (Just black)) $ vBox [viewport MessageLogs logViewport $ messageLog fakeLogs]

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