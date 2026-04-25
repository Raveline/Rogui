module Rogui.Backend.SDLOpenGL
  ( sdlOpenGLBackend,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Foldable
import Data.IORef
import Data.Text (Text)
import Data.Word (Word8)
import Foreign (Storable (..), alloca, with)
import Foreign.C (CChar, CInt (CInt), withCString)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr
import Graphics.GL
import Linear
import Rogui.Backend.Events
import Rogui.Backend.OpenGL.Eval
import Rogui.Backend.OpenGL.Shader
import Rogui.Backend.OpenGL.Types
import Rogui.Backend.Types
import Rogui.Graphics
import qualified SDL
import qualified SDL.Image as SDL
import qualified SDL.Internal.Numbered as Numbered
import qualified SDL.Raw as Raw

foreign import ccall "IMG_SavePNG" imgSavePNG :: Ptr Raw.Surface -> Ptr CChar -> IO CInt

sdlOpenGLBackend :: Backend GLRenderer GLTexture e
sdlOpenGLBackend =
  Backend
    { loadBrush = glLoadBrush,
      clearFrame = glClearFrame,
      presentFrame = glPresentFrame,
      initBackend = initGLBackend,
      evalInstructions = evalGLInstructions,
      getTicks = SDL.ticks,
      pollEvents = getSDLEvents,
      takeScreenshot = glTakeScreenshot
    }

initGLBackend :: (MonadIO m) => Text -> V2 Pixel -> Bool -> (GLRenderer -> m a) -> m ()
initGLBackend appName windowSize allowResize withRenderer = do
  SDL.initializeAll
  refWindowSize <- liftIO $ newIORef (fromIntegral <$> windowSize)
  let glConfig =
        SDL.defaultOpenGL
          { SDL.glProfile = SDL.Core SDL.Normal 3 3
          }

  window <-
    SDL.createWindow
      appName
      SDL.defaultWindow
        { SDL.windowInitialSize = fromIntegral <$> windowSize,
          SDL.windowResizable = allowResize,
          SDL.windowGraphicsContext = SDL.OpenGLContext glConfig
        }
  when allowResize
    . void
    $ SDL.windowMinimumSize window SDL.$= (fromIntegral <$> windowSize)
  glContext <- SDL.glCreateContext window
  SDL.glMakeCurrent window glContext

  -- Compile shaders and link program
  program <- initShaderProgram

  -- Create VAO and VBO
  vao <- genObject glGenVertexArrays
  vbo <- genObject glGenBuffers

  -- Configure VAO with vertex attribute layout
  glBindVertexArray vao
  glBindBuffer GL_ARRAY_BUFFER vbo

  -- Pre-allocate VBO (16384 quads = 98304 vertices)
  let maxVerts = 98304 :: Int
      bufSize = fromIntegral $ maxVerts * vertexSize
  glBufferData GL_ARRAY_BUFFER bufSize nullPtr GL_DYNAMIC_DRAW

  let stride = fromIntegral vertexSize
      floatSz = sizeOf (undefined :: Float)
  -- layout 0: vPos (2 floats at offset 0)
  glEnableVertexAttribArray 0
  glVertexAttribPointer 0 2 GL_FLOAT GL_FALSE stride nullPtr
  -- layout 1: vUV (2 floats at offset 8)
  glEnableVertexAttribArray 1
  glVertexAttribPointer 1 2 GL_FLOAT GL_FALSE stride (nullPtr `plusPtr` (2 * floatSz))
  -- layout 2: vFront (4 floats at offset 16)
  glEnableVertexAttribArray 2
  glVertexAttribPointer 2 4 GL_FLOAT GL_FALSE stride (nullPtr `plusPtr` (4 * floatSz))
  -- layout 3: vBack (4 floats at offset 32)
  glEnableVertexAttribArray 3
  glVertexAttribPointer 3 4 GL_FLOAT GL_FALSE stride (nullPtr `plusPtr` (8 * floatSz))

  glBindVertexArray 0

  -- Default GL state
  glEnable GL_BLEND
  glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
  glDisable GL_DEPTH_TEST

  -- Set projection uniform
  let proj = orthoProjection windowSize
  glUseProgram program
  projLoc <- liftIO $ withCString "uProjection" $ glGetUniformLocation program
  liftIO $ with proj $ \ptr ->
    glUniformMatrix4fv projLoc 1 GL_TRUE (castPtr ptr)

  let glRenderer =
        GLRenderer
          { glWindow = window,
            glContext = glContext,
            glProgram = program,
            glVAO = vao,
            glVBO = vbo,
            glProjection = proj,
            glWindowSize = refWindowSize
          }

  void $ withRenderer glRenderer
  SDL.glDeleteContext glContext
  SDL.destroyWindow window

genObject :: (MonadIO m) => (GLsizei -> Ptr GLuint -> IO ()) -> m GLuint
genObject gen = liftIO $ alloca $ \ptr -> do
  gen 1 ptr
  peek ptr

orthoProjection :: V2 Pixel -> M44 Float
orthoProjection (V2 w h) = ortho 0 (fromIntegral w) (fromIntegral h) 0 (-1) 1

glClearFrame :: (MonadIO m) => GLRenderer -> m ()
glClearFrame _ = do
  glDisable GL_SCISSOR_TEST
  glClearColor 0 0 0 1
  glClear GL_COLOR_BUFFER_BIT

glPresentFrame :: (MonadIO m) => GLRenderer -> m ()
glPresentFrame GLRenderer {..} = do
  SDL.glSwapWindow glWindow

convertSurface :: (MonadIO m) => SDL.Surface -> SDL.PixelFormat -> m SDL.Surface
convertSurface (SDL.Surface s _) pixFmt = do
  fmt <- Raw.allocFormat (Numbered.toNumber pixFmt)
  surface <- SDL.Surface <$> Raw.convertSurface s fmt 0 <*> pure Nothing
  surface <$ Raw.freeFormat fmt

applyTransparency :: (MonadIO m) => Ptr () -> V2 CInt -> RGBA -> m ()
applyTransparency pixels (V2 w h) (V4 tr tg tb _) = liftIO $ do
  let ptr = castPtr pixels :: Ptr Word8
      count = fromIntegral w * fromIntegral h
  for_ [0 .. count - 1] $ \i -> do
    let offset = i * 4 -- 4 bytes per pixel (RGBA8888)
    r <- peekByteOff ptr offset :: IO Word8
    g <- peekByteOff ptr (offset + 1) :: IO Word8
    b <- peekByteOff ptr (offset + 2) :: IO Word8
    when (r == tr && g == tg && b == tb) $
      pokeByteOff ptr (offset + 3) (0 :: Word8) -- set alpha to 0

glLoadBrush :: (MonadIO m) => GLRenderer -> TileSize -> Either ByteString FilePath -> Maybe RGBA -> m (Brush, GLTexture)
glLoadBrush _renderer TileSize {..} method transparency = do
  fontSurface <- case method of
    Left bs -> SDL.decode bs
    Right fp -> SDL.load fp

  surface <- convertSurface fontSurface (SDL.RGBA8888 :: SDL.PixelFormat)

  SDL.lockSurface surface
  pixels <- SDL.surfacePixels surface
  dim@(V2 w h) <- SDL.surfaceDimensions surface
  traverse_ (applyTransparency pixels dim) transparency

  texId <- liftIO $ alloca $ \texPtr -> do
    glGenTextures 1 texPtr -- writes 1 texture name into texPtr
    tex <- peek texPtr -- read it out as a GLuint
    glBindTexture GL_TEXTURE_2D tex
    glTexImage2D GL_TEXTURE_2D 0 GL_RGBA (fromIntegral w) (fromIntegral h) 0 GL_RGBA GL_UNSIGNED_BYTE pixels
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST
    pure tex
  SDL.unlockSurface surface
  SDL.freeSurface surface
  SDL.freeSurface fontSurface

  let texture =
        GLTexture
          { texId = texId,
            texWidth = fromIntegral w,
            texHeight = fromIntegral h
          }

  pure
    ( Brush
        { tileWidth = pixelWidth,
          tileHeight = pixelHeight,
          textureWidth = fromIntegral w,
          textureHeight = fromIntegral h,
          name = either show id method
        },
      texture
    )

glTakeScreenshot :: (MonadIO m) => GLRenderer -> V2 Int -> FilePath -> m ()
glTakeScreenshot _ (V2 w h) fp = liftIO $ do
  let rowBytes = w * 4
      totalBytes = h * rowBytes
  allocaBytes totalBytes $ \buf -> do
    glReadPixels 0 0 (fromIntegral w) (fromIntegral h) GL_RGBA GL_UNSIGNED_BYTE (castPtr buf)
    allocaBytes rowBytes $ \tmp ->
      for_ [0 .. h `div` 2 - 1] $ \y -> do
        let top = buf `plusPtr` (y * rowBytes)
            bot = buf `plusPtr` ((h - 1 - y) * rowBytes)
        copyBytes tmp top rowBytes
        copyBytes top bot rowBytes
        copyBytes bot tmp rowBytes
    surfPtr <-
      Raw.createRGBSurfaceFrom
        (castPtr buf)
        (fromIntegral w)
        (fromIntegral h)
        32
        (fromIntegral rowBytes)
        0xFF000000
        0x00FF0000
        0x0000FF00
        0x000000FF
    withCString fp $ \cFp -> void $ imgSavePNG surfPtr cFp
    Raw.freeSurface surfPtr
