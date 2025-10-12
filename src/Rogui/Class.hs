{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Rogui.Class
  ( GuiRogue (..),
    withWindow,
  )
where

import Control.Monad.State
import qualified Data.Map.Strict as M

-- | A GuiRogue exposes:
-- - A state `s` (with get and put primitives);
-- - A reference type `r` (a label type used to name console)
-- - A monadic stack `m`
class (MonadState s m, Monad m) => GuiRogue s w r m | m -> s w r where
  printAt :: (Int, Int) -> Char -> m ()
  getLayout :: m (M.Map r w)

withWindow :: (GuiRogue s w r m, Ord r) => r -> (w -> m ()) -> m ()
withWindow r f = do
  window <- (M.! r) <$> getLayout
  f window

-- do
-- (Str fc bc txt)