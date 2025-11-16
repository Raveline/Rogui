{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module RogueHarvest.GameModes.Menu
  ( renderMainMenu,
    handleMenuEvents,
  )
where

import Control.Monad (zipWithM_)
import qualified Data.Map.Strict as M
import Lens.Micro.Platform
import RogueHarvest.Constants
import RogueHarvest.Types (GameMode (..), Names (..), PlayingMode (..), RHEvents (..), RogueHarvest (..), currentMode)
import RogueHarvest.Utils (setCurrentMode)
import Rogui.Application
import Rogui.Application.Event.Handlers (focusRingHandler)
import Rogui.Components
import Rogui.FocusRing
import Rogui.Graphics (Colours (..), TextAlign (..), setColours)
import Rogui.Graphics.Colours (gradient, invert)
import Rogui.Graphics.Constants
import Rogui.Graphics.DSL.Instructions (str)
import qualified SDL

renderMainMenu :: FocusRing Names -> Component Names
renderMainMenu ring =
  filled black . bordered bnw $
    vBox
      [ centered . hSize (Fixed 12) $ renderTitle,
        renderOptions ring
      ]

renderTitle :: Component Names
renderTitle =
  let draw' =
        let colourGradient = gradient green yellow (length "Rogueharvest")
            singleChar fc char =
              setColours (Colours (Just fc) (Just black)) >> str TLeft [char]
         in zipWithM_ singleChar colourGradient "Rogueharvest"
   in emptyComponent {draw = draw'}

-- This could very well be rendered through a list.
renderOptions :: FocusRing Names -> Component Names
renderOptions ring =
  centered . hSize (Fixed 12) . vSize (Fixed 4) . bordered bnw $
    vBox
      [ vSize (Fixed 1) $ button MenuNew "(N)ew game" TCenter bnw (invert bnw) (focusGetCurrent ring == Just MenuNew),
        vSize (Fixed 1) $ button MenuQuit "(Q)uit" TCenter bnw (invert bnw) (focusGetCurrent ring == Just MenuQuit)
      ]

handleMenuEvents :: (Monad m) => FocusRing Names -> EventHandler m RogueHarvest RHEvents Names
handleMenuEvents ring = handleKeyEvents ring <||> handleAppEvents

handleMenuFocus :: (Monad m) => FocusRing Names -> EventHandler m RogueHarvest RHEvents Names
handleMenuFocus ring =
  let options =
        M.fromList
          [ (MenuNew, handleButtonEvent (AppEvent NewGame)),
            (MenuQuit, handleButtonEvent Quit)
          ]
   in focusRingHandler options mempty (const ring) (\r -> currentMode .~ Menu r)

handleAppEvents :: (Monad m) => EventHandler m RogueHarvest RHEvents Names
handleAppEvents _ = \case
  (AppEvent NewGame) -> setCurrentMode $ Playing Walking
  _ -> unhandled

handleKeyEvents :: (Monad m) => FocusRing Names -> EventHandler m RogueHarvest RHEvents Names
handleKeyEvents ring =
  let keyMap =
        [ (isSC' SDL.ScancodeQ, \_ _ -> fireEvent Quit),
          (isSC' SDL.ScancodeN, \_ _ -> fireAppEvent NewGame)
        ]
   in keyPressHandler keyMap <||> handleMenuFocus ring