{-# LANGUAGE DuplicateRecordFields #-}

module Rogui.Application.Event
  ( Event (..),
    EventResult (..),
    MouseEventDetails (..),
    KeyDownDetails (..),
    KeyUpDetails (..),
  )
where

import SDL (EventPayload, Keysym, MouseButtonEventData, MouseMotionEventData)

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
  = MouseMove MouseMotionEventData
  | MouseClick MouseButtonEventData

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