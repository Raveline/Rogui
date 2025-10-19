{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (when)
import Control.Monad.Writer
import Data.Array.IArray (Array, genArray, (!))
import Data.Map.Strict qualified as M
import Rogui.Application.Event
import Rogui.Application.System
import Rogui.Application.Types (RoguiConfig (..))
import Rogui.Components.Button (button, handleButtonEvent)
import Rogui.Components.Core
import Rogui.Components.Game.EntitiesLayer (entitiesLayer)
import Rogui.Components.Game.GridTile
import Rogui.Components.Game.Utils (GlyphInfo (..), computeMapViewport)
import Rogui.Components.Label
import Rogui.Components.List
import Rogui.Components.ProgressBar
import Rogui.Components.TextInput
import Rogui.Components.Types
import Rogui.Example
import Rogui.FocusRing
import Rogui.Graphics
import Rogui.Types
import SDL hiding (Event, drawLine, textureHeight, textureWidth)

data State = State
  { gameState :: GameState,
    textValue :: String,
    listOfText :: [String],
    mousePosition :: V2 Cell,
    ring :: FocusRing Name,
    listState :: ListState,
    someText :: String,
    playerPos :: V2 Cell
  }

data GameState = PlayingGame | UI
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
  deriving (Eq)

data CustomEvent = Move (V2 Cell) | ToggleUI

main :: IO ()
main = do
  let config =
        RoguiConfig
          { brushTilesize = TileSize 16 16,
            appName = "RoGUI example",
            consoleCellSize = (V2 50 38),
            targetFPS = 60,
            rootConsoleReference = Root,
            defaultBrushReference = Charset,
            defaultBrushPath = "terminal_16x16.png",
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
        playerPos = V2 1 1
      }

guiMaker :: (MonadIO m) => Rogui Consoles Brushes Name State CustomEvent -> m (Rogui Consoles Brushes Name State CustomEvent)
guiMaker baseGui = do
  let modal = Console {width = 400, height = 200, position = V2 (16 * 10) (16 * 10)}
  withDrawings <- addBrush Drawings "punyworld-dungeon-tileset.png" (TileSize 16 16) baseGui
  pure $ addConsole LittleModal modal withDrawings

uiKeysHandler :: M.Map SDL.Keycode (EventHandler State CustomEvent)
uiKeysHandler =
  M.fromList $
    [ (SDL.KeycodeTab, \_ _ -> fireEvent FocusNext),
      (SDL.KeycodeEscape, \_ _ -> fireAppEvent ToggleUI)
    ]

gameKeysHandler :: M.Map SDL.Keycode (EventHandler State CustomEvent)
gameKeysHandler =
  M.fromList $
    [ (SDL.KeycodeUp, \_ _ -> fireAppEvent . Move $ V2 0 (-1)),
      (SDL.KeycodeDown, \_ _ -> fireAppEvent . Move $ V2 0 1),
      (SDL.KeycodeLeft, \_ _ -> fireAppEvent . Move $ V2 (-1) 0),
      (SDL.KeycodeRight, \_ _ -> fireAppEvent . Move $ V2 1 0),
      (SDL.KeycodeF1, \_ _ -> fireAppEvent $ ToggleUI)
    ]

eventHandler :: EventHandler State CustomEvent
eventHandler state@State {..} e =
  case gameState of
    PlayingGame -> keyPressHandler gameEventHandler gameKeysHandler state e
    UI -> keyPressHandler uiEventHandler uiKeysHandler state e

gameEventHandler :: EventHandler State CustomEvent
gameEventHandler State {..} = \case
  (AppEvent (Move dir)) -> do
    let newPos@(V2 x y) = playerPos + dir
    when (x >= 0 && y >= 0 && x <= 100 && y <= 100) $
      modifyState $
        \s -> s {playerPos = newPos}
  (AppEvent ToggleUI) -> modifyState $ \s -> s {gameState = UI}
  _ -> pure ()

uiEventHandler :: EventHandler State CustomEvent
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

handleFocusChange :: (FocusRing Name -> FocusRing Name) -> State -> EventHandlingM State CustomEvent ()
handleFocusChange ringChange s =
  let newRing = ringChange (ring s)
      -- When focus moves to List and selection is Nothing, initialize
      newListState = case (focusGetCurrent newRing, listState s) of
        (Just List, ListState Nothing offset) -> ListState (Just 0) offset
        _ -> listState s
   in redraw (setCurrentState $ s {ring = newRing, listState = newListState})

renderApp :: M.Map Brushes Brush -> State -> Component Name
renderApp brushes s@State {gameState, playerPos} =
  let tiles = brushes M.! Drawings
      charset = brushes M.! Charset
      baseColours = Colours (Just white) (Just black)
      charColours = Colours (Just white) Nothing
      gridTileSize = (V2 40 30)
      fullMapSize = (V2 100 100)
      viewport = computeMapViewport gridTileSize fullMapSize playerPos
   in case gameState of
        PlayingGame ->
          vBox
            [ vSize (Fixed 1) $
                hBox
                  [ progressBar 0 20 10 baseColours baseColours fullBlock lightShade
                  ],
              multiLayeredGrid fullMapSize viewport $
                [ gridTile tiles gridTileSize ((!) arbitraryMap) tileToGlyphInfo,
                  entitiesLayer charset [playerPos] (const $ GlyphInfo 1 charColours) id
                ]
            ]
        UI -> renderUI s

renderUI :: State -> Component Name
renderUI State {..} =
  let baseColours = Colours (Just white) (Just black)
      highlighted = Colours (Just black) (Just white)
      textColours = Colours (Just white) (Just grey)
      (V2 x y) = mousePosition
   in vBox
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
          button
            "Quit"
            TCenter
            baseColours
            highlighted
            (focusGetCurrent ring == Just QuitButton)
        ]
