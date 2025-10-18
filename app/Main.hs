{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Writer
import Data.Array.IArray (Array, genArray, (!))
import Data.Map.Strict qualified as M
import Rogui.Application.Event
import Rogui.Application.System
import Rogui.Components.Button (button, handleButtonEvent)
import Rogui.Components.Core
import Rogui.Components.GridTile (GlyphInfo (..), gridTile)
import Rogui.Components.Label
import Rogui.Components.List
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
    someText :: String
  }

data GameState = PlayingGame | UI

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
  let generator = \case
        (V2 3 3) -> Wall
        (V2 8 3) -> Wall
        (V2 18 3) -> Wall
        _ -> Floor
   in genArray ((V2 0 0), (V2 20 20)) generator

data Name
  = List
  | TextInput
  | QuitButton
  deriving (Eq)

data CustomEvent

main :: IO ()
main = do
  boot
    (TileSize 16 16)
    "RoGUI example"
    (V2 50 38)
    60
    guiMaker
    $ State
      { gameState = UI,
        textValue = "test",
        listOfText = ["Item 1", "Item 2", "Item 3"],
        mousePosition = V2 0 0,
        ring = focusRing [List, TextInput, QuitButton],
        listState = mkListState {selection = Just 0},
        someText = ""
      }

guiMaker :: (MonadIO m) => SDL.Renderer -> Console -> m (Rogui Consoles Brushes Name State CustomEvent)
guiMaker renderer root = do
  let modal = Console {width = 400, height = 200, position = V2 (16 * 10) (16 * 10)}
  charset <- loadBrush renderer "terminal_16x16.png" (V2 16 16)
  tiles <- loadBrush renderer "punyworld-dungeon-tileset.png" (V2 16 16)
  pure $
    Rogui
      { consoles = M.fromList [(Root, root), (LittleModal, modal)],
        brushes = M.fromList [(Charset, charset), (Drawings, tiles)],
        rootConsole = root,
        defaultBrush = charset,
        draw = renderApp tiles,
        renderer = renderer,
        onEvent = baseEventHandler (keyPressHandler eventHandler keysHandler),
        lastTicks = 0,
        timerStep = 100,
        lastStep = 0,
        numberOfSteps = 0,
        targetFrameTime = 0
      }

keysHandler :: M.Map SDL.Keycode (EventHandler State CustomEvent)
keysHandler =
  M.fromList $
    [(SDL.KeycodeTab, \_ _ -> fireEvent FocusNext)]

handleFocusChange :: (FocusRing Name -> FocusRing Name) -> State -> EventHandlingM State CustomEvent ()
handleFocusChange ringChange s =
  let newRing = ringChange (ring s)
      -- When focus moves to List and selection is Nothing, initialize
      newListState = case (focusGetCurrent newRing, listState s) of
        (Just List, ListState Nothing offset) -> ListState (Just 0) offset
        _ -> listState s
   in redraw (setCurrentState $ s {ring = newRing, listState = newListState})

eventHandler :: EventHandler State CustomEvent
eventHandler state@State {..} = \case
  MouseEvent (MouseMove MouseMoveDetails {..}) ->
    redraw $ setCurrentState $ state {mousePosition = defaultTileSizePosition}
  FocusNext -> handleFocusChange (focusNext) state
  FocusPrev -> handleFocusChange (focusPrev) state
  e -> case focusGetCurrent ring of
    (Just List) -> handleListEvent (length listOfText) e listState (\newLs s -> s {listState = newLs})
    (Just QuitButton) -> handleButtonEvent (\_ _ -> halt (pure ())) state e
    (Just TextInput) -> handleTextInputEvent e someText (\newString s -> s {someText = newString})
    _ -> pure ()

renderApp :: Brush -> State -> Component Name
renderApp tiles s@State {gameState} = case gameState of
  PlayingGame -> gridTile tiles (V2 10 10) (V2 20 20) ((!) arbitraryMap) tileToGlyphInfo (V2 0 0)
  UI -> renderUI s

renderUI :: State -> Component Name
renderUI State {..} =
  let baseColours = Colours (Just white) (Just black)
      highlighted = Colours (Just black) (Just white)
      textColours = Colours (Just white) (Just grey)
      (V2 x y) = mousePosition
   in vBox
        [ ( label
              ("Mouse at: " <> show (x, y))
              TRight
              baseColours
          )
            { vSize = Fixed 1
            },
          ( label
              textValue
              TCenter
              ( Colours
                  (Just red)
                  (Just black)
              )
          )
            { vSize = Fixed 2
            },
          bordered baseColours $ padded 2 $ list listOfText id TLeft baseColours highlighted listState,
          padded 2 $ textInput someText textColours (focusGetCurrent ring == Just TextInput),
          button
            "Quit"
            TCenter
            baseColours
            highlighted
            (focusGetCurrent ring == Just QuitButton)
        ]
