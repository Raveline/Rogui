module Rogui.Backend.OpenGL.Types
  (
    GLRenderer(..)
  , GLTexture(..)
  , Vertex(..)
  , vertexSize
  ) where
import qualified SDL
import Graphics.GL
import Linear
import Data.IORef
import Foreign.Storable
import Foreign

data GLRenderer = GLRenderer
  { glWindow       :: SDL.Window
  , glContext      :: SDL.GLContext
  , glProgram      :: GLuint          -- the sprite shader program
  , glVAO          :: GLuint          -- vertex array object
  , glVBO          :: GLuint          -- vertex buffer object
  , glProjection   :: M44 Float       -- orthographic projection matrix
  , glWindowSize   :: IORef (V2 Int)  -- current window size
  }

-- | A loaded texture on the GPU.
data GLTexture = GLTexture
  { texId     :: GLuint   -- OpenGL texture name
  , texWidth  :: Int      -- total width in pixels
  , texHeight :: Int      -- total height in pixels
  }

data Vertex = Vertex
  { vPos   :: V2 Float   -- screen position (pixels)
  , vUV    :: V2 Float    -- texture coordinate (normalised 0–1)
  , vFront :: V4 Float    -- foreground colour modulation (RGBA, 0–1)
  , vBack  :: V4 Float    -- background colour (RGBA, 0–1; alpha 0 = no bg)
  }

-- | Size of a single vertex in bytes: 2 + 2 + 4 + 4 = 12 floats = 48 bytes.
vertexSize :: Int
vertexSize = 12 * sizeOf (undefined :: Float)

instance Storable Vertex where
  sizeOf _ = vertexSize
  alignment _ = alignment (undefined :: Float)
  peek ptr = do
    let p = castPtr ptr :: Ptr Float
    px  <- peekElemOff p 0
    py  <- peekElemOff p 1
    u   <- peekElemOff p 2
    v   <- peekElemOff p 3
    fr  <- peekElemOff p 4
    fg  <- peekElemOff p 5
    fb  <- peekElemOff p 6
    fa  <- peekElemOff p 7
    br  <- peekElemOff p 8
    bg  <- peekElemOff p 9
    bb  <- peekElemOff p 10
    ba  <- peekElemOff p 11
    pure $ Vertex (V2 px py) (V2 u v) (V4 fr fg fb fa) (V4 br bg bb ba)
  poke ptr (Vertex (V2 px py) (V2 u v) (V4 fr fg fb fa) (V4 br bg bb ba)) = do
    let p = castPtr ptr :: Ptr Float
    pokeElemOff p 0  px
    pokeElemOff p 1  py
    pokeElemOff p 2  u
    pokeElemOff p 3  v
    pokeElemOff p 4  fr
    pokeElemOff p 5  fg
    pokeElemOff p 6  fb
    pokeElemOff p 7  fa
    pokeElemOff p 8  br
    pokeElemOff p 9  bg
    pokeElemOff p 10 bb
    pokeElemOff p 11 ba
