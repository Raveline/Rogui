{-# LANGUAGE DeriveAnyClass #-}

module Rogui.Application.Error
  ( RoguiError (..),
  )
where

import Control.Exception

data RoguiError rc rb
  = NoSuchConsole rc
  | NoSuchBrush rb
  | CannotLoadBrush rb String
  deriving (Eq, Show, Exception)