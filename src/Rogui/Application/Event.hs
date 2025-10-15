{-# LANGUAGE DuplicateRecordFields #-}

module Rogui.Application.Event
  ( Event (..),
    EventResult (..),
    MouseEventDetails (..),
    MouseMoveDetails (..),
    KeyDownDetails (..),
    KeyUpDetails (..),
  )
where

import SDL (EventPayload, Keysym, MouseButtonEventData)
import SDL.Vect (V2)

data Event
  = KeyDown KeyDownDetails
  | KeyUp KeyUpDetails
  | MouseEvent MouseEventDetails
  | OtherSDLEvent EventPayload

data KeyDownDetails = KeyDownDetails
  { repeat :: Bool,
    key :: SDL.Keysym
  }

data KeyUpDetails = KeyUpDetails
  { key :: SDL.Keysym
  }

data MouseEventDetails
  = MouseMove MouseMoveDetails
  | MouseClick MouseButtonEventData

data MouseMoveDetails = MouseMoveDetails
  { relativeMouseMotion :: V2 Int,
    absoluteMousePosition :: V2 Int,
    defaultTileSizePosition :: V2 Int
  }

data EventResult
  = Continue
  | ContinueNoRedraw
  | Halt
  deriving (Eq)

instance Semigroup EventResult where
  _ <> Halt = Halt
  Halt <> _ = Halt
  Continue <> ContinueNoRedraw = Continue
  ContinueNoRedraw <> Continue = Continue
  Continue <> Continue = Continue
  _ <> _ = ContinueNoRedraw