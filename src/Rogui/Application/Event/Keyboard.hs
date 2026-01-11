module Rogui.Application.Event.Keyboard where

data Key
  = KEsc
  | KChar Char
  | KPNum Int
  | KEnter
  | KLeft
  | KRight
  | KUp
  | KDown
  | KCenter
  | KFun Int
  | KTab
  | KBackspace
  | KPrtScr
  | KPause
  | KIns
  | KHome
  | KPageUp
  | KDel
  | KEnd
  | KPageDown
  | KUnknown
  deriving (Eq, Ord)

data Modifier
  = Shift
  | Ctrl
  | Alt
  deriving (Eq, Ord)