{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad (when)
import Linear (V2 (..))
import Rogui.Application.Event
  ( ClickHandler,
    Event (..),
    EventHandler,
    MouseEventDetails (..),
    baseEventHandler,
    foundClickedExtents,
    (<||>),
  )
import Rogui.Application.System (RoguiConfig (..), bootAndPrintError)
import Rogui.Components.Core (Component (..), bordered, emptyComponent, vBox)
import Rogui.Components.List
import Rogui.Graphics
import Rogui.Types (ConsoleDrawers)

data Consoles = Root
  deriving (Show, Eq, Ord)

data Brushes = Charset
  deriving (Show, Eq, Ord)

data Names = DemoList
  deriving (Show, Eq, Ord)

newtype DemoState = DemoState {listState :: ListState}

main :: IO ()
main = do
  let config =
        RoguiConfig
          { brushTilesize = TileSize 10 16,
            appName = "RoGUI list demo",
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
            allowResize = True
          }
  bootAndPrintError
    config
    $ DemoState {listState = mkListState}

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

renderApp :: ConsoleDrawers Consoles Brushes Names DemoState
renderApp _ DemoState {..} =
  let content =
        bordered (Colours (Just white) (Just black))
          . vBox
          $ [ list listDefinition listState
            ]
   in [(Nothing, Nothing, content)]