{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module RogueHarvest.Components.ConfirmDialog
  ( confirmDialog,
    ConfirmDialogState (..),
    ConfirmChoice (..),
    handleConfirmDialogEvent,
  )
where

import Rogui.Application.Event
import Rogui.Components
import Rogui.Graphics
import qualified SDL

-- | The two choices in a confirm dialog
data ConfirmChoice = Yes | No
  deriving (Eq, Show)

-- | State for the confirm dialog (which button is focused)
newtype ConfirmDialogState = ConfirmDialogState
  { focusedChoice :: ConfirmChoice
  }
  deriving (Eq, Show)

-- | A simple confirm dialog with a message and Yes/No buttons.
-- The dialog is rendered with a black background and border.
confirmDialog ::
  (Ord n) =>
  n -> -- Name for Yes button
  n -> -- Name for No button
  String -> -- Message to display
  Colours -> -- Colors for normal state
  Colours -> -- Colors for highlighted state
  ConfirmDialogState -> -- Current state
  Component n
confirmDialog yesName noName message normalColours highlightColours ConfirmDialogState {..} =
  vSize (Fixed 5) $
    hSize (Fixed 20) $
      filled black $
        bordered normalColours $
          vBox
            [ vSize (Fixed 1) $ label message TCenter normalColours,
              vSize (Fixed 1) $ hBox [],
              vSize (Fixed 1) $
                hBox
                  [ button yesName "Yes" TCenter normalColours highlightColours (focusedChoice == Yes),
                    button noName "No" TCenter normalColours highlightColours (focusedChoice == No)
                  ]
            ]

-- | Event handler for confirm dialog navigation
handleConfirmDialogEvent ::
  (Monad m, Ord n) =>
  n -> -- Yes button name
  n -> -- No button name
  ConfirmDialogState ->
  (ConfirmDialogState -> s -> s) -> -- State updater
  (ConfirmChoice -> EventHandlerM m s e n ()) -> -- Action on selection
  EventHandler m s e n
handleConfirmDialogEvent yesName noName state@ConfirmDialogState {..} updater onSelect _ = \case
  KeyDown (KeyDownDetails _ (KeyDetails SDL.KeycodeLeft _ _)) ->
    modifyState $ updater state {focusedChoice = Yes}
  KeyDown (KeyDownDetails _ (KeyDetails SDL.KeycodeRight _ _)) ->
    modifyState $ updater state {focusedChoice = No}
  KeyDown (KeyDownDetails _ (KeyDetails SDL.KeycodeReturn _ _)) ->
    onSelect focusedChoice
  MouseEvent (MouseClickReleased mc) -> do
    clicked <- foundClickedExtents mc
    if yesName `elem` clicked
      then onSelect Yes
      else
        if noName `elem` clicked
          then onSelect No
          else unhandled
  _ -> unhandled
