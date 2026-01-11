{-# LANGUAGE RankNTypes #-}

module Rogui.Backend.Types
  ( Backend (..),
  )
where

import Control.Monad.IO.Class
import Data.ByteString
import qualified Data.Map.Strict as M
import Data.Text
import Data.Word
import Linear
import Rogui.Application.Event (Event)
import Rogui.Graphics

data Backend renderer texture event = Backend
  { loadBrush :: forall m. (MonadIO m) => renderer -> TileSize -> Either ByteString FilePath -> Maybe RGBA -> m (Brush, texture),
    initBackend :: forall m a. (MonadIO m) => Text -> V2 Pixel -> Bool -> (renderer -> m a) -> m (),
    clearFrame :: forall m. (MonadIO m) => renderer -> m (),
    presentFrame :: forall m. (MonadIO m) => renderer -> m (),
    evalInstructions :: forall m. (MonadIO m) => renderer -> M.Map Brush texture -> Console -> Brush -> Instructions -> m (),
    pollEvents :: forall m. (MonadIO m) => Brush -> m [Event event],
    getTicks :: forall m. (MonadIO m) => m Word32
  }