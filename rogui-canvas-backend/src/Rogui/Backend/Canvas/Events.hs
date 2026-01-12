{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module Rogui.Backend.SDL.Events
  ( getSDLEvents,
  )
where

import Control.Monad.IO.Class
import Data.Maybe (catMaybes)
import Data.Set qualified as S
import Rogui.Application.Event
import Rogui.Graphics
import SDL (MouseMotionEventData (MouseMotionEventData))
import SDL qualified
import SDL.Event (MouseButtonEventData (..))

toModifier :: SDL.KeyModifier -> S.Set Modifier
toModifier SDL.KeyModifier {..} =
  S.fromList . catMaybes $
    [ if keyModifierLeftShift then Just Shift else Nothing,
      if keyModifierRightShift then Just Shift else Nothing,
      if keyModifierLeftCtrl then Just Ctrl else Nothing,
      if keyModifierRightCtrl then Just Ctrl else Nothing,
      if keyModifierLeftAlt then Just Alt else Nothing,
      if keyModifierRightAlt then Just Alt else Nothing,
      if keyModifierAltGr then Just Alt else Nothing
    ]

sdlKeyToRoguiKey :: SDL.Keycode -> Key
sdlKeyToRoguiKey kcode = case kcode of
  SDL.KeycodeReturn -> KEnter
  SDL.KeycodeEscape -> KEsc
  SDL.KeycodeBackspace -> KBackspace
  SDL.KeycodeTab -> KTab
  SDL.KeycodeSpace -> KChar ' '
  SDL.KeycodeExclaim -> KChar '!'
  SDL.KeycodeQuoteDbl -> KChar '"'
  SDL.KeycodeHash -> KChar '#'
  SDL.KeycodePercent -> KChar '%'
  SDL.KeycodeDollar -> KChar '$'
  SDL.KeycodeAmpersand -> KChar '&'
  SDL.KeycodeQuote -> KChar '\''
  SDL.KeycodeLeftParen -> KChar '('
  SDL.KeycodeRightParen -> KChar ')'
  SDL.KeycodeAsterisk -> KChar '*'
  SDL.KeycodePlus -> KChar '+'
  SDL.KeycodeComma -> KChar ','
  SDL.KeycodeMinus -> KChar '-'
  SDL.KeycodePeriod -> KChar '.'
  SDL.KeycodeSlash -> KChar '/'
  SDL.Keycode0 -> KChar '0'
  SDL.Keycode1 -> KChar '1'
  SDL.Keycode2 -> KChar '2'
  SDL.Keycode3 -> KChar '3'
  SDL.Keycode4 -> KChar '4'
  SDL.Keycode5 -> KChar '5'
  SDL.Keycode6 -> KChar '6'
  SDL.Keycode7 -> KChar '7'
  SDL.Keycode8 -> KChar '8'
  SDL.Keycode9 -> KChar '9'
  SDL.KeycodeColon -> KChar ':'
  SDL.KeycodeSemicolon -> KChar ';'
  SDL.KeycodeLess -> KChar '<'
  SDL.KeycodeEquals -> KChar '='
  SDL.KeycodeGreater -> KChar '>'
  SDL.KeycodeQuestion -> KChar '?'
  SDL.KeycodeAt -> KChar '@'
  SDL.KeycodeLeftBracket -> KChar '['
  SDL.KeycodeBackslash -> KChar '\\'
  SDL.KeycodeRightBracket -> KChar ']'
  SDL.KeycodeUnderscore -> KChar '_'
  SDL.KeycodeA -> KChar 'a'
  SDL.KeycodeB -> KChar 'b'
  SDL.KeycodeC -> KChar 'c'
  SDL.KeycodeD -> KChar 'd'
  SDL.KeycodeE -> KChar 'e'
  SDL.KeycodeF -> KChar 'f'
  SDL.KeycodeG -> KChar 'g'
  SDL.KeycodeH -> KChar 'h'
  SDL.KeycodeI -> KChar 'i'
  SDL.KeycodeJ -> KChar 'j'
  SDL.KeycodeK -> KChar 'k'
  SDL.KeycodeL -> KChar 'l'
  SDL.KeycodeM -> KChar 'm'
  SDL.KeycodeN -> KChar 'n'
  SDL.KeycodeO -> KChar 'o'
  SDL.KeycodeP -> KChar 'p'
  SDL.KeycodeQ -> KChar 'q'
  SDL.KeycodeR -> KChar 'r'
  SDL.KeycodeS -> KChar 's'
  SDL.KeycodeT -> KChar 't'
  SDL.KeycodeU -> KChar 'u'
  SDL.KeycodeV -> KChar 'v'
  SDL.KeycodeW -> KChar 'w'
  SDL.KeycodeX -> KChar 'x'
  SDL.KeycodeY -> KChar 'y'
  SDL.KeycodeZ -> KChar 'z'
  SDL.KeycodeF1 -> KFun 1
  SDL.KeycodeF2 -> KFun 2
  SDL.KeycodeF3 -> KFun 3
  SDL.KeycodeF4 -> KFun 4
  SDL.KeycodeF5 -> KFun 5
  SDL.KeycodeF6 -> KFun 6
  SDL.KeycodeF7 -> KFun 7
  SDL.KeycodeF8 -> KFun 8
  SDL.KeycodeF9 -> KFun 9
  SDL.KeycodeF10 -> KFun 10
  SDL.KeycodeF11 -> KFun 11
  SDL.KeycodeF12 -> KFun 12
  SDL.KeycodePrintScreen -> KPrtScr
  SDL.KeycodePause -> KPause
  SDL.KeycodeInsert -> KIns
  SDL.KeycodeHome -> KHome
  SDL.KeycodePageUp -> KPageUp
  SDL.KeycodeDelete -> KDel
  SDL.KeycodeEnd -> KEnd
  SDL.KeycodePageDown -> KPageDown
  SDL.KeycodeRight -> KRight
  SDL.KeycodeLeft -> KLeft
  SDL.KeycodeDown -> KDown
  SDL.KeycodeUp -> KUp
  SDL.KeycodeKPDivide -> KChar '/'
  SDL.KeycodeKPMultiply -> KChar '*'
  SDL.KeycodeKPMinus -> KChar '-'
  SDL.KeycodeKPPlus -> KChar '+'
  SDL.KeycodeKPEnter -> KEnter
  SDL.KeycodeKP1 -> KPNum 1
  SDL.KeycodeKP2 -> KPNum 2
  SDL.KeycodeKP3 -> KPNum 3
  SDL.KeycodeKP4 -> KPNum 4
  SDL.KeycodeKP5 -> KPNum 5
  SDL.KeycodeKP6 -> KPNum 6
  SDL.KeycodeKP7 -> KPNum 7
  SDL.KeycodeKP8 -> KPNum 8
  SDL.KeycodeKP9 -> KPNum 9
  SDL.KeycodeKP0 -> KPNum 0
  SDL.KeycodeKPPeriod -> KChar '.'
  SDL.KeycodeKPEquals -> KChar '='
  SDL.KeycodeF13 -> KFun 13
  SDL.KeycodeF14 -> KFun 14
  SDL.KeycodeF15 -> KFun 15
  SDL.KeycodeF16 -> KFun 16
  SDL.KeycodeF17 -> KFun 17
  SDL.KeycodeF18 -> KFun 18
  SDL.KeycodeF19 -> KFun 19
  SDL.KeycodeF20 -> KFun 20
  SDL.KeycodeF21 -> KFun 21
  SDL.KeycodeF22 -> KFun 22
  SDL.KeycodeF23 -> KFun 23
  SDL.KeycodeF24 -> KFun 24
  SDL.KeycodeKPComma -> KChar ','
  _ -> KUnknown

getSDLEvents :: (MonadIO m) => Brush -> m [Event e]
getSDLEvents Brush {..} =
  let toRoguiEvent = \case
        SDL.QuitEvent -> Quit
        SDL.WindowShownEvent _ -> WindowVisible
        SDL.KeyboardEvent ke -> case SDL.keyboardEventKeyMotion ke of
          SDL.Pressed -> KeyDown $ KeyDownDetails (SDL.keyboardEventRepeat ke) (keysymToKeyDetails $ SDL.keyboardEventKeysym ke)
          SDL.Released -> KeyUp . keysymToKeyDetails $ SDL.keyboardEventKeysym ke
        SDL.WindowSizeChangedEvent (SDL.WindowSizeChangedEventData _ (SDL.V2 w h)) ->
          WindowResized (SDL.V2 (fromIntegral w ./.= tileWidth) (fromIntegral h ./.= tileHeight))
        SDL.MouseMotionEvent MouseMotionEventData {..} ->
          let (SDL.P mousePos) = mouseMotionEventPos
              absoluteMousePosition@(SDL.V2 x y) = fromIntegral <$> mousePos
              defaultTileSizePosition = SDL.V2 (x ./.= tileWidth) (y ./.= tileHeight)
              relativeMouseMotion = fromIntegral <$> mouseMotionEventRelMotion
           in MouseEvent . MouseMove $ MouseMoveDetails {..}
        SDL.MouseButtonEvent MouseButtonEventData {..} ->
          let (SDL.P mousePos) = mouseButtonEventPos
              absoluteMousePosition@(SDL.V2 x y) = fromIntegral <$> mousePos
              defaultTileSizePosition = SDL.V2 (x ./.= tileWidth) (y ./.= tileHeight)
              buttonClicked = sdlButtonToMouseButton mouseButtonEventButton
              constructor = if mouseButtonEventMotion == SDL.Released then MouseClickReleased else MouseClickPressed
           in MouseEvent . constructor $ MouseClickDetails {..}
        _ -> UnknownEvent
      deduplicatedPayload es = S.toList . S.fromList $ fmap SDL.eventPayload es
   in fmap (fmap toRoguiEvent) (deduplicatedPayload <$> SDL.pollEvents)

sdlButtonToMouseButton :: SDL.MouseButton -> MouseButton
sdlButtonToMouseButton = \case
  SDL.ButtonLeft -> LeftButton
  SDL.ButtonRight -> RightButton
  _ -> MiddleButton

keysymToKeyDetails :: SDL.Keysym -> KeyDetails
keysymToKeyDetails SDL.Keysym {..} =
  KeyDetails (sdlKeyToRoguiKey keysymKeycode) (toModifier keysymModifier)
