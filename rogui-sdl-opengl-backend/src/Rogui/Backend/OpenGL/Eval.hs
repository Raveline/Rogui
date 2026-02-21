{-# LANGUAGE NamedFieldPuns #-}
module Rogui.Backend.OpenGL.Eval
  (
    evalGLInstructions
  ) where
import Control.Monad.IO.Class
import Rogui.Backend.OpenGL.Types
import qualified Data.Map.Strict as M
import Rogui.Graphics hiding (Console(..))
import qualified Rogui.Graphics as R (Console(..))
import Linear
import Data.Maybe
import Control.Monad.State
import Data.Foldable
import Control.Monad
import Graphics.GL
import Foreign hiding (void)
import Foreign.C (withCString)
import Rogui.Backend.OpenGL.Primitives
import Data.Char
import qualified Data.DList as D

data DrawingState = DrawingState
  { console :: !R.Console,
    brush :: !Brush,
    texture :: GLTexture,
    position :: !(V2 Cell),
    renderer :: !GLRenderer,
    colours :: !Colours,
    verts :: D.DList Vertex,
    useTileset :: !Bool
  }

-- | Using the given render, default console and default brush, apply a set
-- of instructions.
evalGLInstructions :: (MonadIO m) => GLRenderer -> M.Map Brush GLTexture -> R.Console -> Brush -> Instructions -> m ()
evalGLInstructions renderer knownTextures console brush instructions = do
  let position = V2 0 0
      colours = Colours {front = Nothing, back = Nothing}
      texture = fromMaybe (error $ "Unknown texture for " <> show brush) $ brush `M.lookup` knownTextures
      verts = D.empty
      useTileset = True
  finalState <- execStateT (traverse_ (eval knownTextures) instructions) (DrawingState {..})
  flushBatch finalState

flushBatch :: (MonadIO m) => DrawingState -> m ()
flushBatch DrawingState{..} =
  let vertList = D.toList verts
   in unless (null vertList) $ liftIO $
        withArrayLen vertList $ \len ptr -> do
          let byteSize = fromIntegral $ len * vertexSize
          -- Bind shader, VAO, texture
          glUseProgram (glProgram renderer)
          glBindVertexArray (glVAO renderer)
          glActiveTexture GL_TEXTURE0
          glBindTexture GL_TEXTURE_2D (texId texture)
          -- Set uUseTileset uniform
          uTilesetLoc <- withCString "uUseTileset" $ glGetUniformLocation (glProgram renderer)
          glUniform1i uTilesetLoc (if useTileset then 1 else 0)
          -- Upload vertex data
          glBindBuffer GL_ARRAY_BUFFER (glVBO renderer)
          glBufferSubData GL_ARRAY_BUFFER 0 byteSize (castPtr ptr)
          -- Draw
          glDrawArrays GL_TRIANGLES 0 (fromIntegral len)
          glBindVertexArray 0

resetBatch :: (MonadState DrawingState m) => m ()
resetBatch = modify (\s -> s {verts = D.empty})

applyBlendMode :: (MonadIO m) => Maybe BlendMode -> m ()
applyBlendMode Nothing = pure ()
applyBlendMode (Just AlphaBlend) = do
  glEnable GL_BLEND
  glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
applyBlendMode (Just Add) = do
  glEnable GL_BLEND
  glBlendFunc GL_SRC_ALPHA GL_ONE
applyBlendMode (Just None) =
  glDisable GL_BLEND

restoreDefaultBlend :: (MonadIO m) => m ()
restoreDefaultBlend = do
  glEnable GL_BLEND
  glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

eval :: (MonadState DrawingState m, MonadIO m) => M.Map Brush GLTexture -> Instruction -> m ()
eval textures instruction = do
  DrawingState {console, brush, renderer, texture, colours, position} <- get
  let flush = get >>= flushBatch >> resetBatch
      enqueueVertices glyphTrans glyphId pos =
        let newVerts = printCharAt console brush texture glyphTrans colours glyphId pos
         in modify (\s -> s {verts = verts s <> D.fromList newVerts})
  case instruction of
    OnConsole newConsole -> do
      flush
      modify (\s -> s {console = newConsole})
      clipToConsole renderer newConsole
    WithBrush newBrush ->
      let newTexture = fromMaybe (error $ "Unknown texture for " <> show newBrush) $ newBrush `M.lookup` textures
       in do flush
             modify (\s -> s {brush = newBrush, texture = newTexture})
    DrawBorder -> do
      let R.Console {width, height} = console
          Brush {..} = brush
          (w, h) = (width ./.= tileWidth - 1, height ./.= tileHeight - 1)
          draw = enqueueVertices []
          bottoms = [V2 x y | x <- [1 .. w - 1], y <- [h]]
          tops = [V2 x y | x <- [1 .. w - 1], y <- [0]]
          lefts = [V2 x y | x <- [0], y <- [1 .. h - 1]]
          rights = [V2 x y | x <- [w], y <- [1 .. h - 1]]
      traverse_ (draw horizontal437) tops
      traverse_ (draw horizontal437) bottoms
      traverse_ (draw vertical437) lefts
      traverse_ (draw vertical437) rights
      draw cornerTopLeft437 $ V2 0 0
      draw cornerBottomLeft437 $ V2 0 h
      draw cornerTopRight437 $ V2 w 0
      draw cornerBottomRight437 $ V2 w h
    DrawString alignment' str' -> do
      let drawOne n = enqueueVertices [] (ord n)
          basePos = case alignment' of
            TCenter -> position - V2 (Cell $ length str' `div` 2) 0
            TRight -> position - V2 (Cell $ length str') 0
            _ -> position
          next (i, c) = drawOne c (basePos + (Cell <$> V2 1 0 ^* i))
          indexed = zip [0 ..] str'
       in traverse_ next indexed
    SetConsoleBackground rgb -> do
      flush
      modify (\s -> s {verts = D.fromList (fillConsoleWith console rgb), useTileset = False})
      get >>= flushBatch >> resetBatch
      modify (\s -> s {useTileset = True})
    NewLine ->
      let (V2 px _) = position
       in modify (\s -> s {position = position + V2 (-px) 1})
    DrawGlyph glyphId trans ->
      enqueueVertices trans glyphId position
    MoveTo pos ->
      modify (\s -> s {position = pos})
    MoveBy by ->
      modify (\s -> s {position = position + by})
    SetColours col ->
      modify (\s -> s {colours = col})
    OverlayAt at colour mode -> do
      flush
      applyBlendMode mode
      modify (\s -> s {verts = D.fromList (overlayRect console brush at (V2 1 1) colour), useTileset = False})
      get >>= flushBatch >> resetBatch
      modify (\s -> s {useTileset = True})
      restoreDefaultBlend
    DrawGlyphAts ats glyphId ->
      traverse_ (enqueueVertices [] glyphId) ats
    FullConsoleOverlay colour mode -> do
      flush
      applyBlendMode mode
      modify (\s -> s {verts = D.fromList (overlayRect console brush (V2 0 0) (V2 (R.width console ./.= tileWidth brush) (R.height console ./.= tileHeight brush)) colour), useTileset = False})
      get >>= flushBatch >> resetBatch
      modify (\s -> s {useTileset = True})
      restoreDefaultBlend
